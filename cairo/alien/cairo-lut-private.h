/* Copyright 2006 Google Inc. All Rights Reserved.
 * amcrae@google.com (Andrew McRae)
 * Converted from cairo-lut.c
 * Author: yangzh@google.com (Zhonghao Yang)
 *
 * Inlines and defines for pre-multiplied alpha color conversion.
 * The naming convention is:
 *   rgb - separate values for red, green, blue
 *   argb - separate values for alpha, red, green, blue
 *   pixel - 32 bit combined value, no premultiplication
 *   rgbpixel - 24 bit RGB, no alpha.
 *   cairo - pixel that has alpha premultiplication
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

#ifndef CAIRO_LUT_PRIVATE_H
#define CAIRO_LUT_PRIVATE_H

extern const uint8_t color_lut[256][256];
extern const uint8_t reverse_color_lut[256][256];

static inline uint8_t _get_alpha (uint32_t pixel)
{
  return (pixel >> 24) & 0xFF;
}

static inline uint8_t _get_red (uint32_t pixel)
{
  return (pixel >> 16) & 0xFF;
}

static inline uint8_t _get_green (uint32_t pixel)
{
  return (pixel >> 8) & 0xFF;
}

static inline uint8_t _get_blue (uint32_t pixel)
{
  return pixel & 0xFF;
}

static inline uint32_t
rgb_to_pixel (uint8_t r, uint8_t g, uint8_t b)
{
  return ((r << 16) + (g << 8) + b);
}

static inline uint32_t
argb_to_pixel (uint8_t alpha, uint8_t r, uint8_t g, uint8_t b)
{
  return ((alpha << 24) + rgb_to_pixel(r, g, b));
}

static inline void
pixel_to_rgb (uint32_t pixel, uint8_t *r, uint8_t *g, uint8_t *b)
{
  *r = _get_red(pixel);
  *g = _get_green(pixel);
  *b = _get_blue(pixel);
}

/*
 * alpha_cairo_to_rgb:
 * @al: The alpha value to apply to the pixel
 * @pixel: the regular RGB pixel value
 *
 * return regular R/G/B channel. (non-pre-multiplied)
 *
 */
static inline void
alpha_cairo_to_rgb (uint8_t al, uint32_t pixel,
                    uint8_t *r, uint8_t *g, uint8_t *b)
{
  if (al != 0xFF) {
    if (al == 0) {
      *r = 0;
      *g = 0;
      *b = 0;
    } else {
      *r = color_lut[al][_get_red(pixel)];
      *g = color_lut[al][_get_green(pixel)];
      *b = color_lut[al][_get_blue(pixel)];
    }
  } else {
    pixel_to_rgb(pixel, r, g, b);
  }
}

/*
 * cairo_to_rgb:
 * @pixel: the regular ARGB pixel value
 *
 * return regular R/G/B channel. (non-pre-multiplied)
 *
 */
static inline void
cairo_to_rgb (uint32_t pixel, uint8_t *r, uint8_t *g, uint8_t *b)
{
  alpha_cairo_to_rgb(_get_alpha(pixel), pixel, r, g, b);
}

/*
 * cairo_to_pixel:
 * @pixel: the premultiplied ARGB pixel value
 *
 * return regular Alpha/R/G/B channel. (non-pre-multiplied)
 *
 */
static inline uint32_t
cairo_to_pixel (uint32_t pixel)
{
  uint8_t alpha, r, g, b;
  alpha = _get_alpha (pixel);
  
  if (alpha != 0xFF) {
    if (alpha == 0) {
      return(0);
    }
    r = color_lut[alpha][_get_red(pixel)];
    g = color_lut[alpha][_get_green(pixel)];
    b = color_lut[alpha][_get_blue(pixel)];
    pixel = argb_to_pixel(alpha, r, g, b);
  }
  return(pixel);
}

/*
 * cairo_to_rgbpixel:
 * @pixel: the premultiplied ARGB pixel value
 *
 * return R/G/B channel.
 *
 */
static inline uint32_t
cairo_to_rgbpixel (uint32_t pixel)
{
    return(cairo_to_pixel(pixel) & 0xFFFFFF);
}

/*
 * argb_to_cairo:
 * @a: the alpha value
 * @r: the red value
 * @g: the green value
 * @b: the blue value
 *
 * given a regular ARGB, return corresponding pre-multipled pixel value.
 * this is the reverse function for cairo_to_rgb.
 *
 * Return value: internal pre-multiplied pixel value.
 *
 */
static inline uint32_t
argb_to_cairo (uint8_t a, uint8_t r, uint8_t g, uint8_t b)
{
    if (a != 0xFF) {
        r = reverse_color_lut[a][r];
        g = reverse_color_lut[a][g];
        b = reverse_color_lut[a][b];
    }
    return argb_to_pixel (a, r, g, b);
}

/*
 * pixel_to_cairo:
 * @pixel_r: the regular ARGB pixel value.
 *
 * given a regular ARGB, return corresponding pre-multipled pixel value.
 * this is the reverse function for cairo_to_pixel.
 *
 * Return value: internal pre-multiplied pixel value.
 *
 */
static inline uint32_t
pixel_to_cairo (uint32_t pixel_r)
{
  uint8_t alpha;

  alpha = _get_alpha (pixel_r);
  if (alpha == 0xFF)
    return pixel_r;

  return argb_to_cairo (_get_alpha(pixel_r), 
                        _get_red(pixel_r),
                        _get_green(pixel_r),
                        _get_blue(pixel_r));
}

#endif /* CAIRO_LUT_PRIVATE_H */
