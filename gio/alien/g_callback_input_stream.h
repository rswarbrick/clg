#ifndef __G_CALLBACK_INPUT_STREAM_H__
#define __G_CALLBACK_INPUT_STREAM_H__

#include <gio/gio.h>

G_BEGIN_DECLS

#define G_TYPE_CALLBACK_INPUT_STREAM         (g_callback_input_stream_get_type ())
#define G_CALLBACK_INPUT_STREAM(o)           (G_TYPE_CHECK_INSTANCE_CAST ((o), G_TYPE_CALLBACK_INPUT_STREAM, GCallbackInputStream))
#define G_CALLBACK_INPUT_STREAM_CLASS(k)     (G_TYPE_CHECK_CLASS_CAST((k), G_TYPE_CALLBACK_INPUT_STREAM, GCallbackInputStreamClass))
#define G_IS_CALLBACK_INPUT_STREAM(o)        (G_TYPE_CHECK_INSTANCE_TYPE ((o), G_TYPE_CALLBACK_INPUT_STREAM))
#define G_IS_CALLBACK_INPUT_STREAM_CLASS(k)  (G_TYPE_CHECK_CLASS_TYPE ((k), G_TYPE_CALLBACK_INPUT_STREAM))
#define G_CALLBACK_INPUT_STREAM_GET_CLASS(o) (G_TYPE_INSTANCE_GET_CLASS ((o), G_TYPE_CALLBACK_INPUT_STREAM, GCallbackInputStreamClass))


typedef struct _GCallbackInputStream         GCallbackInputStream;
typedef struct _GCallbackInputStreamClass    GCallbackInputStreamClass;
typedef struct _GCallbackInputStreamPrivate  GCallbackInputStreamPrivate;

struct _GCallbackInputStream
{
  GInputStream parent_instance;

  /*< private >*/
  GCallbackInputStreamPrivate *priv;
};

struct _GCallbackInputStreamClass
{
  GInputStreamClass parent_class;
};

GType g_callback_input_stream_get_type (void) G_GNUC_CONST;

typedef gssize (*GCallbackInputStreamReadFunc) (void *buffer,
						gsize count,
						GCancellable *cancellable,
						GError **error,
						gpointer data);

typedef gboolean (*GCallbackInputStreamCloseFunc) (GCancellable *cancellable,
						   GError **error,
						   gpointer data);


GInputStream*
g_callback_input_stream_new (GCallbackInputStreamReadFunc read_func,
			     GCallbackInputStreamCloseFunc close_func,
			     gpointer user_data);

G_END_DECLS

#endif /* __G_CALLBACK_INPUT_STREAM_H__ */
