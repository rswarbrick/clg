/* Common Lisp bindings for GTK+ v2.x
 * Copyright 1999-2005 Espen S. Johnsen <espen@users.sf.net>
 *
 * Permission is hereby granted, free of charge, to any person obtaining
 * a copy of this software and associated documentation files (the
 * "Software"), to deal in the Software without restriction, including
 * without limitation the rights to use, copy, modify, merge, publish,
 * distribute, sublicense, and/or sell copies of the Software, and to
 * permit persons to whom the Software is furnished to do so, subject to
 * the following conditions:
 *
 * The above copyright notice and this permission notice shall be
 * included in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
 * IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
 * CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
 * TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
 * SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

/* $Id: glue.c,v 1.9 2007-09-06 14:27:07 espen Exp $ */


#include <gtk/gtk.h>


/*
 *
 * Gtk helper functions
 *
 */

void
gtk_query_version (guint *major, guint *minor, guint *micro)
{
  *major = gtk_major_version;
  *minor = gtk_minor_version;
  *micro = gtk_micro_version;
}


/* Widget */

GdkWindow*
gtk_widget_get_window (GtkWidget *widget)
{
  return widget->window;
}

GtkStateType
gtk_widget_get_state (GtkWidget *widget)
{
  return widget->state;
}

gint32
gtk_widget_flags (GtkWidget *widget)
{
  return GTK_WIDGET_FLAGS (widget);
}


void
gtk_widget_get_size_allocation (GtkWidget *widget, int *width, int *height)
{
  *width = widget->allocation.width;
  *height = widget->allocation.height;
}


/* Container */

GtkWidget*
gtk_container_get_focus_child (GtkContainer *container)
{
  return container->focus_child;
}

gboolean
gtk_container_get_reallocate_redraws (GtkContainer *container)
{
  return container->reallocate_redraws;
}


/* Dialog */

GtkWidget*
gtk_dialog_get_vbox (GtkDialog *dialog)
{
  return dialog->vbox;
}

GtkWidget*
gtk_dialog_get_action_area (GtkDialog *dialog)
{
  return dialog->action_area;
}



/* Window */

GtkWidget*
gtk_window_get_default (GtkWindow *window)
{
  return window->default_widget;
}

GtkWindowGroup*
gtk_window_get_group (GtkWindow *window)
{
  return window->group;
}



/* Menu */

GdkScreen*
gtk_menu_get_screen (GtkMenu *menu)
{
  return (GdkScreen*)g_object_get_data (G_OBJECT (menu), "gtk-menu-explicit-screen");
}



/* Toolbar */

GtkTooltips*
gtk_toolbar_get_tooltips_object (GtkToolbar *toolbar)
{
  return toolbar->tooltips;
}



/* Tooltips */

gint
gtk_tooltips_get_enabled (GtkTooltips *tooltips)
{
  return tooltips->enabled;
}


/* Layout */

GdkWindow*
gtk_layout_get_bin_window (GtkLayout *layout)
{
  return layout->bin_window;
}


/* GtkStyle accessor functions */

typedef enum {
  GTK_COLOR_FG,
  GTK_COLOR_BG,
  GTK_COLOR_LIGHT,
  GTK_COLOR_DARK,
  GTK_COLOR_MID,
  GTK_COLOR_TEXT,
  GTK_COLOR_BASE,
  GTK_COLOR_TEXT_AA,
  GTK_COLOR_WHITE,
  GTK_COLOR_BLACK
} GtkColorType;

GdkColor*
gtk_style_get_color (GtkStyle *style, GtkColorType color_type,
		     GtkStateType state)
{
  switch (color_type)
    {
    case GTK_COLOR_WHITE:
      return &style->white;
    case GTK_COLOR_BLACK:
      return &style->black;
    case GTK_COLOR_FG:
      return &style->fg[state];
    case GTK_COLOR_BG:
      return &style->bg[state];
    case GTK_COLOR_LIGHT:
      return &style->light[state];
    case GTK_COLOR_DARK:
      return &style->dark[state];
    case GTK_COLOR_MID:
      return &style->mid[state];
    case GTK_COLOR_TEXT:
      return &style->text[state];
    case GTK_COLOR_BASE:
      return &style->base[state];
    case GTK_COLOR_TEXT_AA:
      return &style->text_aa[state];
    }
}


void
gtk_style_set_color (GtkStyle *style, GtkColorType color_type,
		     GtkStateType state, GdkColor *color)
{
  switch (color_type)
    {
    case GTK_COLOR_WHITE:
      style->white = *color; break;
    case GTK_COLOR_BLACK:
      style->black = *color; break;
    case GTK_COLOR_FG:
      style->fg[state] = *color; break;
    case GTK_COLOR_BG:
      style->bg[state]  = *color; break;
    case GTK_COLOR_LIGHT:
      style->light[state]  = *color; break;
    case GTK_COLOR_DARK:
      style->dark[state]  = *color; break;
    case GTK_COLOR_MID:
      style->mid[state]  = *color; break;
    case GTK_COLOR_TEXT:
      style->text[state]  = *color; break;
    case GTK_COLOR_BASE:
      style->base[state]  = *color; break;
    case GTK_COLOR_TEXT_AA:
      style->text_aa[state]  = *color; break;
    }
}


GdkGC*
gtk_style_get_gc (GtkStyle *style, GtkColorType color_type, GtkStateType state)
{
  switch (color_type)
    {
    case GTK_COLOR_WHITE:
      return style->white_gc;
    case GTK_COLOR_BLACK:
      return style->black_gc;
    case GTK_COLOR_FG:
      return style->fg_gc[state];
    case GTK_COLOR_BG:
      return style->bg_gc[state];
    case GTK_COLOR_LIGHT:
      return style->light_gc[state];
    case GTK_COLOR_DARK:
      return style->dark_gc[state];
    case GTK_COLOR_MID:
      return style->mid_gc[state];
    case GTK_COLOR_TEXT:
      return style->text_gc[state];
    case GTK_COLOR_BASE:
      return style->base_gc[state];
    case GTK_COLOR_TEXT_AA:
      return style->text_aa_gc[state];
    }
}

int
gtk_style_font_desc_offset ()
{
  GtkStyle style;
  
  return (int)&style.font_desc - (int)&style;
}

