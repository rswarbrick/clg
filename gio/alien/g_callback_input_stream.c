#include "g_callback_input_stream.h"

G_DEFINE_TYPE (GCallbackInputStream, g_callback_input_stream, G_TYPE_INPUT_STREAM);

struct _GCallbackInputStreamPrivate {
  GCallbackInputStreamReadFunc read_func;
  GCallbackInputStreamCloseFunc close_func;
  gpointer user_data;
};

static gssize   g_callback_input_stream_read  (GInputStream *stream,
					       void         *buffer,
					       gsize        count,
					       GCancellable *cancellable,
					       GError       **error);
static gssize   g_callback_input_stream_skip  (GInputStream *stream,
					       gsize        count,
					       GCancellable *cancellable,
					       GError       **error);
static gboolean g_callback_input_stream_close (GInputStream *stream,
					       GCancellable *cancellable,
					       GError       **error);


static void
g_callback_input_stream_finalize (GObject *object)
{
  GCallbackInputStream *stream;
  
  stream = G_CALLBACK_INPUT_STREAM (object);

  G_OBJECT_CLASS (g_callback_input_stream_parent_class)->finalize (object);
}

static void
g_callback_input_stream_class_init (GCallbackInputStreamClass *klass)
{
  GObjectClass *gobject_class = G_OBJECT_CLASS (klass);
  GInputStreamClass *stream_class = G_INPUT_STREAM_CLASS (klass);
  
  g_type_class_add_private (klass, sizeof (GCallbackInputStreamPrivate));
  
  gobject_class->finalize = g_callback_input_stream_finalize;

  stream_class->read_fn = g_callback_input_stream_read;
  stream_class->skip = g_callback_input_stream_skip;
  stream_class->close_fn = g_callback_input_stream_close;
}

static void
g_callback_input_stream_init (GCallbackInputStream *callback_stream)
{
  callback_stream->priv =
    G_TYPE_INSTANCE_GET_PRIVATE (callback_stream,
				 G_TYPE_CALLBACK_INPUT_STREAM,
				 GCallbackInputStreamPrivate);
}

GInputStream*
g_callback_input_stream_new (GCallbackInputStreamReadFunc read_func,
			     GCallbackInputStreamCloseFunc close_func,
			     gpointer user_data)
{
  GCallbackInputStream *stream;

  stream = g_object_new (G_TYPE_CALLBACK_INPUT_STREAM, NULL);

  stream->priv->read_func = read_func;
  stream->priv->close_func = close_func;
  stream->priv->user_data = user_data;
  
  return G_INPUT_STREAM (stream);
}

static gssize
g_callback_input_stream_read (GInputStream  *stream,
			      void          *buffer,
			      gsize          count,
			      GCancellable  *cancellable,
			      GError       **error)
{
  GCallbackInputStream *callback_stream = G_CALLBACK_INPUT_STREAM (stream);
  GCallbackInputStreamReadFunc read_func = callback_stream->priv->read_func;
  gpointer user_data = callback_stream->priv->user_data;

  return read_func (buffer, count, cancellable, error, user_data);
}

static gssize
g_callback_input_stream_skip (GInputStream  *stream,
			      gsize          count,
			      GCancellable  *cancellable,
			      GError       **error)
{
  GCallbackInputStream *callback_stream = G_CALLBACK_INPUT_STREAM (stream);
  GCallbackInputStreamReadFunc read_func = callback_stream->priv->read_func;
  gpointer user_data = callback_stream->priv->user_data;

  return read_func (NULL, count, cancellable, error, user_data);
}

static gboolean
g_callback_input_stream_close (GInputStream  *stream,
			       GCancellable  *cancellable,
			       GError       **error)
{
  GCallbackInputStream *callback_stream = G_CALLBACK_INPUT_STREAM (stream);
  GCallbackInputStreamCloseFunc close_func = callback_stream->priv->close_func;
  gpointer user_data = callback_stream->priv->user_data;

  callback_stream = G_CALLBACK_INPUT_STREAM (stream);

  if (callback_stream->priv->close_func)
    return close_func (cancellable, error, user_data);

  return TRUE;
}

#define __G_CALLBACK_INPUT_STREAM_C__

