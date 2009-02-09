/* Modified for clg by esj */

/* Copyright 2005 Google Inc. All Rights Reserved.
 * Author: yangzh@google.com (Zhonghao Yang)
 *
 * Google's own patch to add jpeg I/O support for cairo.
 *
 */

/* cairo - a vector graphics library with display and print output
 *
 * Copyright © 2003 University of Southern California
 *
 * This library is free software; you can redistribute it and/or
 * modify it either under the terms of the GNU Lesser General Public
 * License version 2.1 as published by the Free Software Foundation
 * (the "LGPL") or, at your option, under the terms of the Mozilla
 * Public License Version 1.1 (the "MPL"). If you do not alter this
 * notice, a recipient may use your version of this file under either
 * the MPL or the LGPL.
 *
 * You should have received a copy of the LGPL along with this library
 * in the file COPYING-LGPL-2.1; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 * You should have received a copy of the MPL along with this library
 * in the file COPYING-MPL-1.1
 *
 * The contents of this file are subject to the Mozilla Public License
 * Version 1.1 (the "License"); you may not use this file except in
 * compliance with the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * This software is distributed on an "AS IS" basis, WITHOUT WARRANTY
 * OF ANY KIND, either express or implied. See the LGPL or the MPL for
 * the specific language governing rights and limitations.
 *
 * The Original Code is the cairo graphics library.
 *
 * The Initial Developer of the Original Code is University of Southern
 * California.
 *
 * Contributor(s):
 *	Carl D. Worth <cworth@cworth.org>
 *	Kristian Høgsberg <krh@redhat.com>
 */

#include <stdio.h>
#include <stdint.h>
#include <string.h>
#include <stdlib.h>
#include <setjmp.h>
#include <errno.h>
#include <jpeglib.h>
#include <cairo.h>
#include "cairo-lut-private.h"
#include "cairo-prebuilt-lut.h"

#define BUF_SIZE_JPEG 4096

struct clg_error_mgr {
  struct jpeg_error_mgr pub;
  jmp_buf setjmp_buffer;
};
	
typedef struct clg_error_mgr *error_ptr;
	
static void
_error_exit (j_common_ptr cinfo)
{
  error_ptr err = (error_ptr) cinfo->err;
	
  (*cinfo->err->output_message) (cinfo);
  longjmp(err->setjmp_buffer, 1);
}


typedef unsigned int (*cairo_jpeg_read_func_t)
                      (void *closure,
	               unsigned char *data,
	               unsigned int  length);

struct read_closure {
  /**
   * Model underlying system's read behavior:
   * data can be read from stdio, stream, or anything.
   */
  cairo_jpeg_read_func_t jpeg_func;

  /**
   * Auxiliary data:
   * For stdio, this should be struct file_closure *
   */
  void *user_data;
  /*
   * Buffer for reading from
   */
  unsigned char block[BUF_SIZE_JPEG];

  cairo_status_t status;
};

/*
 * Read function for stdio.
 * Check length of read and return error if necessary.
 */
static
unsigned int stdio_read_func (void *closure,
                              unsigned char *data,
                              unsigned int length)
{
  return fread((void *)data, 1, length, (FILE *)closure);
}

/*
 * Initialize source.
 * called by jpeg_read_header before any data is actually read.
 */
static void init_source_custom (j_decompress_ptr cinfo)
{
  cinfo->src->bytes_in_buffer = 0;
}

/*
 * Fill the input buffer.
 * called whenever buffer is emptied.
 */
static boolean fill_input_buffer_custom (j_decompress_ptr cinfo)
{
  struct read_closure *closure = (struct read_closure*) (cinfo->client_data);
  unsigned int len;

  len = (*closure->jpeg_func) (closure->user_data,
                               closure->block, BUF_SIZE_JPEG);

  if (len == 0) {
    /* really hit EOF in the beginning: insert a fake EOI marker. */
    static const unsigned char jpeg_eof[] = { 0xFF, JPEG_EOI };
    cinfo->src->bytes_in_buffer = 2;
    cinfo->src->next_input_byte = jpeg_eof;
  } else {
      cinfo->src->bytes_in_buffer = len;
      cinfo->src->next_input_byte = closure->block;
  }
  return TRUE;
}


/*
 * Skip data.
 * used to skip over a potentially large amount of uninteresting
 * data (such as APPn marker).
 * Writers of suspendable-input applications must note that skip_input_data
 * is not granted the right to give a suspension return.  If the skip extends
 * beyond the data currently in the buffer, the buffer can be marked empty so
 * that the next read will cause a fill_input_buffer call that can suspend.
 * Arranging for additional bytes to be discarded before reloading the input
 * buffer is the application writer's problem.
 */
static void skip_input_data_custom (j_decompress_ptr cinfo, long num_bytes)
{
  while (num_bytes > (long) (cinfo->src->bytes_in_buffer)) {
    num_bytes -= (long) (cinfo->src->bytes_in_buffer);
    fill_input_buffer_custom (cinfo);
  }

  cinfo->src->next_input_byte += (size_t) num_bytes;
  cinfo->src->bytes_in_buffer -= (size_t) num_bytes;
}

/*
 * Terminate source.
 * called by jpeg_finish_decompress after all data has been read.
 * Often a no-op.
 */
static void term_source_custom (j_decompress_ptr cinfo)
{
}

/*
 * read jpeg from any source.
 */
static cairo_surface_t *
read_jpeg (struct read_closure *closure)
{
  cairo_surface_t *surface = NULL;

  struct jpeg_decompress_struct cinfo;
  struct clg_error_mgr jerr;
  struct jpeg_source_mgr src;

  int channels;
  uint8_t *data;
  int stride;
  
  JSAMPROW row = NULL;
  JSAMPROW rowptr[1];

  register JSAMPROW src_pixel;
  register uint32_t *dst_pixel;
  unsigned int i, j;

  closure->status = CAIRO_STATUS_SUCCESS;
  
  /* Step 1: Allocate and initialize a JPEG decompression object */
  /* Step 2: Specify the source of the compressed data (eg, a file) */
  memset (&cinfo, 0, sizeof (cinfo));
  memset (&jerr, 0, sizeof (jerr));

  cinfo.err = jpeg_std_error (&jerr.pub);
  jerr.pub.error_exit = _error_exit;
  if (setjmp(jerr.setjmp_buffer))
    goto ERROR;

  jpeg_create_decompress (&cinfo);

  cinfo.client_data = closure;

  src.init_source = init_source_custom;
  src.fill_input_buffer = fill_input_buffer_custom;
  src.skip_input_data = skip_input_data_custom;
  src.resync_to_restart = jpeg_resync_to_restart;  /* JPEG library default */
  src.term_source = term_source_custom;
  src.bytes_in_buffer = 0;  /* forces fill_input_buffer on first read. */
  src.next_input_byte = NULL;	/* until buffer loaded. */
  cinfo.src = &src;

  /* save the APP14 marker to check for Adobe Photoshop CMYK */
  /* files with inverted components. */
  jpeg_save_markers (&cinfo, JPEG_APP0 + 14, 256);

  /* Step 3: Call jpeg_read_header() to obtain image info. */
  if (jpeg_read_header (&cinfo, TRUE) != JPEG_HEADER_OK)
    goto ERROR;

  /* if (cinfo.image_height > INT_MAX || cinfo.image_width > INT_MAX) */
  /*   goto BAIL1; */

  /* NOTE(yangzh): do not support these two yet. */
  if (cinfo.jpeg_color_space == JCS_CMYK || cinfo.jpeg_color_space == JCS_YCCK)
    goto ERROR;
    
  cinfo.out_color_space = JCS_RGB;

  /* the fastest, but less accurate integer method for DCT. */
  /* not recommende if high quality is a concern. we do not have this issue. */
  cinfo.dct_method = JDCT_IFAST;

  /* Step 4: Set target cairo image.
   * for CAIRO_FORMAT_ARGB32, every pixel will take 4 bytes.
   */
  surface = cairo_image_surface_create (CAIRO_FORMAT_ARGB32,
					cinfo.image_width,
					cinfo.image_height);
  if (cairo_surface_status (surface) != CAIRO_STATUS_SUCCESS) {
    closure->status = cairo_surface_status (surface);
    goto ERROR;
  }

  /* Step 5: jpeg_start_decompress(...); */
  jpeg_start_decompress (&cinfo);

  /* Step 6: while (scan lines remain to be read) */
  /*           jpeg_read_scanlines(...); */
  channels = 3;
  row = (JSAMPROW) malloc (cinfo.output_width * channels * sizeof (JSAMPLE));
  if (row == NULL) {
    closure->status = CAIRO_STATUS_NO_MEMORY;
    goto ERROR;
  }

  data = cairo_image_surface_get_data (surface);
  stride = cairo_image_surface_get_stride (surface);
  
  rowptr[0] = row;
  for (i = 0; i < cinfo.output_height; i++) {
    jpeg_read_scanlines (&cinfo, rowptr, 1);
    src_pixel = row;
    dst_pixel = (uint32_t*)(data + i * stride);
    for (j = 0; j < cinfo.output_width; j++, src_pixel += channels, dst_pixel++) {
      /* Store ARGB in native endian. */
      *dst_pixel = (0xFF << 24)   /* always 0xFF in alpha channel. */
                   + (src_pixel[0] << 16)
                   + (src_pixel[1] << 8)
                   + (src_pixel[2]);
    }
  }

  /* Step 7: jpeg_finish_decompress(...); */
  jpeg_finish_decompress (&cinfo);
  goto EXIT;
  
 ERROR:
  if (surface) {
    cairo_surface_destroy (surface);
    surface = NULL;
  }
  if (closure->status == CAIRO_STATUS_SUCCESS)
    closure->status = CAIRO_STATUS_READ_ERROR;

 EXIT:
  /* Release other buffers used in this function. */
  if (row)
    free (row);

  /* Step 8: Release the JPEG decompression object. */
  jpeg_destroy_decompress (&cinfo);

  return surface;
}

cairo_surface_t *
cairo_image_surface_create_from_jpeg (const char *filename,
				      cairo_status_t *status)
{
  struct read_closure jpeg_closure;
  FILE *fp;
  cairo_surface_t *surface;

  fp = fopen (filename, "rb");
  if (fp == NULL) {
    switch (errno) {
      case ENOMEM:
        *status = CAIRO_STATUS_NO_MEMORY;
        return NULL;
      case ENOENT:
        *status = CAIRO_STATUS_FILE_NOT_FOUND;
        return NULL;
      default:
        *status = CAIRO_STATUS_READ_ERROR;
        return NULL;
    }
  }

  jpeg_closure.jpeg_func = stdio_read_func;
  jpeg_closure.user_data = (void*) fp;
  
  surface = read_jpeg (&jpeg_closure);
  fclose (fp);

  *status = jpeg_closure.status;
  return surface;
}

cairo_surface_t *
cairo_image_surface_create_from_jpeg_stream (cairo_jpeg_read_func_t read_func,
                                             void *closure,
					     cairo_status_t *status)
{
  struct read_closure jpeg_closure;
  jpeg_closure.jpeg_func = read_func;
  jpeg_closure.user_data = closure;

  cairo_surface_t *surface = read_jpeg (&jpeg_closure);

  *status = jpeg_closure.status;
  return surface;
}


typedef struct _cairo_jpeg_parameter {
  int quality;
  cairo_bool_t interlace;
} cairo_jpeg_parameter_t;

void cairo_get_default_jpeg_parameter (cairo_jpeg_parameter_t *param);

typedef unsigned int (*cairo_jpeg_write_func_t)
                      (void *closure,
	               const unsigned char *data,
	               unsigned int length);

struct write_closure {
  cairo_jpeg_write_func_t jpeg_func;

  /**
   * Auxiliary data:
   * For stdio, this should be struct file_closure *
   */
  void *user_data;
  /*
   * Buffer for writing to.
   */
  unsigned char block[BUF_SIZE_JPEG];
};

/*
 * write func for stdio.
 * since closure_data->byte_written contains enough diagnostic information,
 * we always return SUCCESS here.
 */
static
cairo_status_t stdio_write_func (void *closure,
                                 const unsigned char *data,
                                 unsigned int length)
{
  int bytes_written;
  do {
    bytes_written = fwrite(data, 1, length, (FILE *)closure);
    data += bytes_written;
    length -= bytes_written;
  } while (bytes_written > 0 && length > 0);
  
  return CAIRO_STATUS_SUCCESS;  
}

/*
 * Initialize destination.
 * called by jpeg_start_compress before any data is actually written.
 */
static void init_destination_jpeg_custom (j_compress_ptr cinfo)
{
  struct write_closure *closure = (struct write_closure*) (cinfo->client_data);
  cinfo->dest->next_output_byte = closure->block;
  cinfo->dest->free_in_buffer = BUF_SIZE_JPEG;
}

/*
 * Empty buffer.
 * called whenever buffer fills up.
 * In typical applications, this should write the entire output buffer
 * (ignoring the current state of next_output_byte & free_in_buffer),
 * reset the pointer & count to the start of the buffer, and return TRUE
 * indicating that the buffer has been dumped.
 *
 * In applications that need to be able to suspend compression due to output
 * overrun, a FALSE return indicates that the buffer cannot be emptied now.
 * In this situation, the compressor will return to its caller (possibly with
 * an indication that it has not accepted all the supplied scanlines).  The
 * application should resume compression after it has made more room in the
 * output buffer.  Note that there are substantial restrictions on the use of
 * suspension --- see the documentation.
 *
 * When suspending, the compressor will back up to a convenient restart point
 * (typically the start of the current MCU). next_output_byte & free_in_buffer
 * indicate where the restart point will be if the current call returns FALSE.
 * Data beyond this point will be regenerated after resumption, so do not
 * write it out when emptying the buffer externally.
 */
static boolean empty_output_buffer_jpeg_custom (j_compress_ptr cinfo)
{
  struct write_closure *closure = (struct write_closure*) (cinfo->client_data);
  int len;
  len = (*closure->jpeg_func) (closure->user_data,
                               closure->block, BUF_SIZE_JPEG);
  if (len != BUF_SIZE_JPEG) {
    /* seems not all remaining bytes in buffer has been dumped yet. */
    /* by return FALSE, we reply on JPEG's error handling machnism?  */
    return FALSE;
  }
  cinfo->dest->next_output_byte = closure->block;
  cinfo->dest->free_in_buffer = BUF_SIZE_JPEG;
  return TRUE;
}

/*
 * Terminate destination.
 * called by jpeg_finish_compress afer all data has been written.
 * NB: *not* called by jpeg_abort or jpeg_destroy; surrounding
 * application must deal with any cleanup that should happen even
 * for error exit.
 */
static void term_destination_jpeg_custom (j_compress_ptr cinfo)
{
  struct write_closure *closure = (struct write_closure*) (cinfo->client_data);
  unsigned int len;
  len = BUF_SIZE_JPEG - cinfo->dest->free_in_buffer;
  if (len > 0)
    (*closure->jpeg_func) (closure->user_data, closure->block, len);
}

static cairo_status_t
write_jpeg (cairo_surface_t *surface, void* closure,
            const cairo_jpeg_parameter_t *parameter)
{
  cairo_status_t status = CAIRO_STATUS_SUCCESS;

  struct jpeg_compress_struct cinfo;
  struct clg_error_mgr jerr;
  struct jpeg_destination_mgr dest;
  cairo_jpeg_parameter_t *param, default_parameters;

  JSAMPROW row = NULL;
  JSAMPROW rowptr[1];

  register uint32_t *src_pixel;
  register uint8_t *src_pixel_gray;
  register JSAMPROW dst_pixel;
  unsigned int i, j;
  int convert_alpha;

  param = (cairo_jpeg_parameter_t *)parameter;
  if (param == NULL) {
      param = &default_parameters;
      cairo_get_default_jpeg_parameter(param);
  }


  /* Step 1:  Allocate and initialize a JPEG compression object. */
  memset (&cinfo, 0, sizeof (cinfo));
  memset (&jerr, 0, sizeof (jerr));

  cinfo.err = jpeg_std_error (&jerr.pub);
  jerr.pub.error_exit = _error_exit;
  if (setjmp(jerr.setjmp_buffer))
    goto BAIL;

  jpeg_create_compress (&cinfo);

  /* Step 2:  Specify the destination for the compressed data. */
  cinfo.client_data = closure;

  dest.init_destination = init_destination_jpeg_custom;
  dest.empty_output_buffer = empty_output_buffer_jpeg_custom;
  dest.term_destination = term_destination_jpeg_custom;
  dest.next_output_byte = NULL;
  dest.free_in_buffer = 0;
  cinfo.dest = &dest;

  /* Step 3: Set parameters for compression, image size & colorspace, etc. */
  cinfo.image_width = cairo_image_surface_get_width (surface);
  cinfo.image_height = cairo_image_surface_get_height (surface);
  convert_alpha = 0;

  switch (cairo_image_surface_get_format (surface)) {
    case CAIRO_FORMAT_RGB24:
      cinfo.input_components = 3;	/* # of color components per pixel */
      cinfo.in_color_space = JCS_RGB;	/* colorspace of input image */
      break;
    case CAIRO_FORMAT_ARGB32:
      cinfo.input_components = 3;	/* # of color components per pixel */
      cinfo.in_color_space = JCS_RGB;	/* colorspace of input image */
      convert_alpha = 1;
      break;
    case CAIRO_FORMAT_A8: /* Put A channel into a grayscale JPEG image. */
    case CAIRO_FORMAT_A1: /* Put A channel into a "binary" JPEG image. */
      cinfo.input_components = 1;
      cinfo.in_color_space = JCS_GRAYSCALE;
    default:
      status = CAIRO_STATUS_NULL_POINTER;
      goto BAIL;
  }

  jpeg_set_defaults (&cinfo);  /* set compression parameters all default values. */

  /* the fastest, but less accurate integer method for DCT. */
  /* not recommende if high quality is a concern. we do not have this issue. */
  cinfo.dct_method = JDCT_IFAST;

  if (param->quality >= 0)
    jpeg_set_quality(&cinfo, param->quality, TRUE);

  /* If user requests interlace, translate that to progressive JPEG */
  if (param->interlace)
    jpeg_simple_progression(&cinfo);

  row = (JSAMPROW) malloc (cinfo.image_width * cinfo.input_components
			   * sizeof (JSAMPLE));
  if (row == NULL) {
    status = CAIRO_STATUS_NO_MEMORY;
    goto BAIL;
  }

  rowptr[0] = row;

  /* Step 4:  jpeg_start_compress(...); */
  jpeg_start_compress (&cinfo, TRUE);

  /* Step 5:  while (scan lines remain to be written) */
  /*		jpeg_write_scanlines(...); */
  uint8_t *data = cairo_image_surface_get_data (surface);
  int stride = cairo_image_surface_get_stride (surface);
  
  if (cinfo.input_components == 3) {  /* truecolor JPEG. */
    for (i = 0; i < cinfo.image_height; i++) {
      src_pixel = (uint32_t *) (data + i * stride);
      dst_pixel = row;
      /*
       * If the pixels have an alpha channel, convert
       * the pixels to normal RGB
       */
      if (convert_alpha) {
        for (j = 0; j < cinfo.image_width; j++, dst_pixel += 3) {
          uint8_t r, g, b;
          cairo_to_rgb(*src_pixel++, &r, &g, &b);
          dst_pixel[0] = r;
          dst_pixel[1] = g;
          dst_pixel[2] = b;
        }
      } else {
        for (j = 0; j < cinfo.image_width; j++, dst_pixel += 3) {
          unsigned int pix;
          pix = *src_pixel++;
          dst_pixel[0] = _get_red(pix);
          dst_pixel[1] = _get_green(pix);
          dst_pixel[2] = _get_blue(pix);
        }
      }
      jpeg_write_scanlines (&cinfo, rowptr, 1);
    }
  } else {  /* write to a grayscale JPEG. */
    src_pixel_gray = (uint8_t *) data;
    for (i = 0; i < cinfo.image_height; i++) {
      memcpy (row, src_pixel_gray, cinfo.image_width);
      src_pixel_gray += stride;
      jpeg_write_scanlines (&cinfo, rowptr, 1);
    }
  }

  /* Step 6:  jpeg_finish_compress(...); */
  jpeg_finish_compress (&cinfo);

 BAIL:
  if (row)
    free (row);

  /* Step 7:  Release the JPEG compression object. */
  jpeg_destroy_compress (&cinfo);

  return status;
}

void
cairo_get_default_jpeg_parameter (cairo_jpeg_parameter_t *param)
{
  param->quality = 75;
  param->interlace = TRUE;
}

cairo_status_t
cairo_surface_write_to_jpeg (cairo_surface_t *surface,
                             const char *filename,
                             const cairo_jpeg_parameter_t *parameter)
{
  FILE *fp;
  struct write_closure jpeg_closure;
  cairo_status_t status;

  fp = fopen (filename, "wb");
  if (fp == NULL)
    return CAIRO_STATUS_WRITE_ERROR;

  jpeg_closure.jpeg_func = stdio_write_func;
  jpeg_closure.user_data = fp;

  status = write_jpeg (surface, &jpeg_closure, parameter);
  if (fclose(fp) < 0)
    status = CAIRO_STATUS_WRITE_ERROR;

  return status;
}

cairo_status_t
cairo_surface_write_to_jpeg_stream (cairo_surface_t *surface,
                                    cairo_jpeg_write_func_t write_func,
                                    void *closure,
				    const cairo_jpeg_parameter_t *parameter)
{
  struct write_closure jpeg_closure;
  jpeg_closure.jpeg_func = write_func;
  jpeg_closure.user_data = closure;

  return write_jpeg (surface, &jpeg_closure, parameter);
}
