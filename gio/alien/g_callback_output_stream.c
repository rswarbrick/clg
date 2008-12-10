#include "g_callback_output_stream.h"

G_DEFINE_TYPE (GCallbackOutputStream, g_callback_output_stream, G_TYPE_OUTPUT_STREAM);

struct _GCallbackOutputStreamPrivate {
  GCallbackOutputStreamWriteFunc write_func;
  GCallbackOutputStreamFlushFunc flush_func;
  GCallbackOutputStreamCloseFunc close_func;
  gpointer user_data;
};

static gssize   g_callback_output_stream_write  (GOutputStream *stream,
						 void const    *buffer,
						 gsize          count,
						 GCancellable  *cancellable,
						 GError       **error);
static gboolean g_callback_output_stream_flush  (GOutputStream *stream,
						 GCancellable  *cancellable,
						 GError       **error);
static gboolean g_callback_output_stream_close (GOutputStream *stream,
						GCancellable  *cancellable,
						GError       **error);


static void
g_callback_output_stream_finalize (GObject *object)
{
  GCallbackOutputStream *stream;
  
  stream = G_CALLBACK_OUTPUT_STREAM (object);

  G_OBJECT_CLASS (g_callback_output_stream_parent_class)->finalize (object);
}

static void
g_callback_output_stream_class_init (GCallbackOutputStreamClass *klass)
{
  GObjectClass *gobject_class = G_OBJECT_CLASS (klass);
  GOutputStreamClass *stream_class = G_OUTPUT_STREAM_CLASS (klass);
  
  g_type_class_add_private (klass, sizeof (GCallbackOutputStreamPrivate));
  
  gobject_class->finalize = g_callback_output_stream_finalize;

  stream_class->write_fn = g_callback_output_stream_write;
  stream_class->flush = g_callback_output_stream_flush;
  stream_class->close_fn = g_callback_output_stream_close;
}

static void
g_callback_output_stream_init (GCallbackOutputStream *callback_stream)
{
  callback_stream->priv =
    G_TYPE_INSTANCE_GET_PRIVATE (callback_stream,
				 G_TYPE_CALLBACK_OUTPUT_STREAM,
				 GCallbackOutputStreamPrivate);
}

GOutputStream*
g_callback_output_stream_new (GCallbackOutputStreamWriteFunc write_func,
			      GCallbackOutputStreamFlushFunc flush_func,
			      GCallbackOutputStreamCloseFunc close_func,
			      gpointer user_data)
{
  GCallbackOutputStream *stream;

  stream = g_object_new (G_TYPE_CALLBACK_OUTPUT_STREAM, NULL);

  stream->priv->write_func = write_func;
  stream->priv->flush_func = flush_func;
  stream->priv->close_func = close_func;
  stream->priv->user_data = user_data;
  
  return G_OUTPUT_STREAM (stream);
}

static gssize
g_callback_output_stream_write (GOutputStream  *stream,
				const void     *buffer,
				gsize           count,
				GCancellable   *cancellable,
				GError        **error)
{
  GCallbackOutputStream *callback_stream = G_CALLBACK_OUTPUT_STREAM (stream);
  GCallbackOutputStreamWriteFunc write_func = callback_stream->priv->write_func;
  gpointer user_data = callback_stream->priv->user_data;

  return write_func (buffer, count, cancellable, error, user_data);
}

static gboolean
g_callback_output_stream_flush (GOutputStream  *stream,
				GCancellable  *cancellable,
				GError       **error)
{
  GCallbackOutputStream *callback_stream = G_CALLBACK_OUTPUT_STREAM (stream);
  GCallbackOutputStreamFlushFunc flush_func = callback_stream->priv->flush_func;
  gpointer user_data = callback_stream->priv->user_data;

  if (callback_stream->priv->flush_func)
    return flush_func (cancellable, error, user_data);

  return TRUE;
}

static gboolean
g_callback_output_stream_close (GOutputStream  *stream,
			       GCancellable  *cancellable,
			       GError       **error)
{
  GCallbackOutputStream *callback_stream = G_CALLBACK_OUTPUT_STREAM (stream);
  GCallbackOutputStreamCloseFunc close_func = callback_stream->priv->close_func;
  gpointer user_data = callback_stream->priv->user_data;

  callback_stream = G_CALLBACK_OUTPUT_STREAM (stream);

  if (callback_stream->priv->close_func)
    return close_func (cancellable, error, user_data);

  return TRUE;
}

#define __G_CALLBACK_OUTPUT_STREAM_C__

