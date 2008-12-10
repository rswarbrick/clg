#ifndef __G_CALLBACK_OUTPUT_STREAM_H__
#define __G_CALLBACK_OUTPUT_STREAM_H__

#include <gio/gio.h>

G_BEGIN_DECLS

#define G_TYPE_CALLBACK_OUTPUT_STREAM         (g_callback_output_stream_get_type ())
#define G_CALLBACK_OUTPUT_STREAM(o)           (G_TYPE_CHECK_INSTANCE_CAST ((o), G_TYPE_CALLBACK_OUTPUT_STREAM, GCallbackOutputStream))
#define G_CALLBACK_OUTPUT_STREAM_CLASS(k)     (G_TYPE_CHECK_CLASS_CAST((k), G_TYPE_CALLBACK_OUTPUT_STREAM, GCallbackOutputStreamClass))
#define G_IS_CALLBACK_OUTPUT_STREAM(o)        (G_TYPE_CHECK_INSTANCE_TYPE ((o), G_TYPE_CALLBACK_OUTPUT_STREAM))
#define G_IS_CALLBACK_OUTPUT_STREAM_CLASS(k)  (G_TYPE_CHECK_CLASS_TYPE ((k), G_TYPE_CALLBACK_OUTPUT_STREAM))
#define G_CALLBACK_OUTPUT_STREAM_GET_CLASS(o) (G_TYPE_INSTANCE_GET_CLASS ((o), G_TYPE_CALLBACK_OUTPUT_STREAM, GCallbackOutputStreamClass))


typedef struct _GCallbackOutputStream         GCallbackOutputStream;
typedef struct _GCallbackOutputStreamClass    GCallbackOutputStreamClass;
typedef struct _GCallbackOutputStreamPrivate  GCallbackOutputStreamPrivate;

struct _GCallbackOutputStream
{
  GOutputStream parent_instance;

  /*< private >*/
  GCallbackOutputStreamPrivate *priv;
};

struct _GCallbackOutputStreamClass
{
  GOutputStreamClass parent_class;
};

GType g_callback_output_stream_get_type (void) G_GNUC_CONST;

typedef gssize (*GCallbackOutputStreamWriteFunc) (const void *buffer,
						  gsize count,
						  GCancellable *cancellable,
						  GError **error,
						  gpointer data);

typedef gboolean (*GCallbackOutputStreamFlushFunc) (GCancellable *cancellable,
						    GError **error,
						    gpointer data);

typedef gboolean (*GCallbackOutputStreamCloseFunc) (GCancellable *cancellable,
						    GError **error,
						    gpointer data);


GOutputStream*
g_callback_output_stream_new (GCallbackOutputStreamWriteFunc read_func,
			      GCallbackOutputStreamFlushFunc flush_func,
			      GCallbackOutputStreamCloseFunc close_func,
			      gpointer user_data);

G_END_DECLS

#endif /* __G_CALLBACK_OUTPUT_STREAM_H__ */
