/* Common Lisp bindings for GTK+ v2.0
 * Copyright (C) 1999-2002 Espen S. Johnsen <espen@users.sourceforge.net>
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */

/* $Id: gtkglue.c,v 1.10 2002-03-24 13:01:12 espen Exp $ */


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


void gtk_callback_marshal (GtkWidget *widget, gpointer data)
{
  GValue arg;

  memset (&arg, 0, sizeof (GValue));
  g_value_init (&arg, gtk_widget_get_type ());
  g_value_set_object (&arg, widget);
  callback_marshal ((guint)data, NULL, 1, &arg);
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

gboolean
gtk_widget_mapped_p (GtkWidget *widget)
{
  return GTK_WIDGET_MAPPED (widget);
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



/* Menu item */

GtkSubmenuPlacement
gtk_menu_item_get_placement (GtkMenuItem* menu_item)
{
  return menu_item->submenu_placement;
}

gint
gtk_menu_item_get_show_submenu (GtkMenuItem* menu_item)
{
  return menu_item->show_submenu_indicator;
}

void
gtk_menu_item_set_show_submenu (GtkMenuItem* menu_item, guint show)
{
  menu_item->show_submenu_indicator = show;
}



/* Check menu item */

gboolean
gtk_check_menu_item_get_active (GtkCheckMenuItem* check_menu_item)
{
  return check_menu_item->active;
}

gboolean
gtk_check_menu_item_get_show_toggle (GtkCheckMenuItem* check_menu_item)
{
  return check_menu_item->always_show_toggle;
}


/* Window */

void
gtk_window_wmclass (GtkWindow* window, gchar* name, gchar* class)
{
  name = window->wmclass_name;
  class = window->wmclass_class;
}


/* File selection */

GtkWidget*
gtk_file_selection_get_action_area (GtkFileSelection *filesel)
{
  return filesel->action_area;
}

GtkWidget*
gtk_file_selection_get_ok_button (GtkFileSelection *filesel)
{
  return filesel->ok_button;
}

GtkWidget*
gtk_file_selection_get_cancel_button (GtkFileSelection *filesel)
{
  return filesel->cancel_button;
}


/* Color selection */

gtk_color_selection_set_color_by_values (GtkColorSelection *colorsel,
					 gdouble red,
					 gdouble green,
					 gdouble blue,
					 gdouble opacity)
{
  gdouble color[4];

  color[0] = red;
  color[1] = green;
  color[2] = blue;
  color[3] = opacity;

  gtk_color_selection_set_color (colorsel, color);
}

void
gtk_color_selection_get_color_as_values (GtkColorSelection *colorsel,
					 gdouble *red,
					 gdouble *green,
					 gdouble *blue,
					 gdouble *opacity)
{
  gdouble color[4];

  gtk_color_selection_get_color (colorsel, color);

  *red = color[0];
  *green = color[1];
  *blue = color[2];
  *opacity = color[3];
}


/* Combo */

GtkWidget*
gtk_combo_get_entry (GtkCombo *combo)
{
  return combo->entry;
}

gboolean
gtk_combo_get_use_arrows (GtkCombo *combo)
{
  return combo->use_arrows;
}

gboolean
gtk_combo_get_use_arrows_always (GtkCombo *combo)
{
  return combo->use_arrows_always;
}

gboolean
gtk_combo_get_case_sensitive (GtkCombo *combo)
{
  return combo->case_sensitive;
}


/* Paned */

GtkWidget*
gtk_paned_child1 (GtkPaned *paned, guint *resize, guint *shrink)
{
  *resize = paned->child1_resize;
  *shrink = paned->child1_shrink;
  
  return paned->child1;
}


GtkWidget*
gtk_paned_child2 (GtkPaned *paned, guint *resize, guint *shrink)
{
  *resize = paned->child2_resize;
  *shrink = paned->child2_shrink;
  
  return paned->child2;
}

gint
gtk_paned_get_position (GtkPaned *paned)
{
  if (paned->position_set == TRUE) 
    return paned->child1_size;
  else
    return -1;
}


/* Layout */

GdkWindow*
gtk_layout_get_bin_window (GtkLayout *layout)
{
  return layout->bin_window;
}


/* List */

GList*
gtk_list_selection (GtkList *list)
{
  return list->selection;
}



/* Toolbar */

gint
gtk_toolbar_num_children (GtkToolbar *toolbar)
{
  return toolbar->num_children;
}

gint
gtk_toolbar_get_tooltips (GtkToolbar *toolbar)
{
  return toolbar->tooltips->enabled;
}


/* Drawing area */

void
gtk_drawing_area_get_size (GtkDrawingArea *darea, gint *width, gint *height)
{
  GtkWidget *widget;

  widget = GTK_WIDGET (darea);
  *width = widget->allocation.width;
  *height = widget->allocation.height;
}


/* Progress */

gchar*
gtk_progress_get_format_string (GtkProgress *progress)
{
  return progress->format;
}

GtkAdjustment*
gtk_progress_get_adjustment (GtkProgress *progress)
{
  return progress->adjustment;
}


/* Tooltips */

gboolean
gtk_tooltips_get_enabled (GtkTooltips *tooltips)
{
  return tooltips->enabled;
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
    }
}


GdkColor*
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
    }

  return gtk_style_get_color (style, color_type, state);
}

/*
GdkFont*
gtk_style_get_font (GtkStyle *style)
{
  return style->font;
}


GdkFont*
gtk_style_set_font (GtkStyle *style, GdkFont *font)
{
  return style->font = font;
}
*/

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
    }
}
