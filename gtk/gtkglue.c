/* Common Lisp bindings for GTK+ v2.0
 * Copyright (C) 1999-2000 Espen S. Johnsen <esj@stud.cs.uit.no>
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

/* $Id: gtkglue.c,v 1.4 2000-11-09 20:30:16 espen Exp $ */


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


/* Is this necessary? */

GtkType
gtk_object_type (GtkObject *obj)
{
  return GTK_OBJECT_TYPE (obj);
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
gtk_widget_allocation (GtkWidget *widget, int *width, int *height)
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



/* Menu item */

GtkWidget*
gtk_menu_item_get_submenu (GtkMenuItem* menu_item)
{
  return menu_item->submenu;
}

GtkSubmenuPlacement
gtk_menu_item_get_placement (GtkMenuItem* menu_item)
{
  return menu_item->submenu_placement;
}

gint
gtk_menu_item_get_show_toggle (GtkMenuItem* menu_item)
{
  return menu_item->show_toggle_indicator;
}

gint
gtk_menu_item_get_show_submenu (GtkMenuItem* menu_item)
{
  return menu_item->show_submenu_indicator;
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


/* Tree item */

GtkWidget*
gtk_tree_item_get_subtree (GtkTreeItem* tree_item)
{
  return GTK_TREE_ITEM_SUBTREE (tree_item);
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


/* CList */

#ifdef CLIST
GList*
gtk_clist_selection (GtkCList *clist)
{
  return clist->selection;
}

gint
gtk_clist_get_titles_visible (GtkCList *clist)
{
  return (clist->title_window && GTK_WIDGET_VISIBLE (clist->title_window));
}

gint
gtk_clist_get_n_rows (GtkCList *clist)
{
  return clist->rows;
}

gint
gtk_clist_get_focus_row (GtkCList *clist)
{
  return clist->focus_row;
}

gint
gtk_clist_get_sort_column (GtkCList *clist)
{
  return clist->sort_column;
}

GtkJustification
gtk_clist_column_justification (GtkCList *clist,
				 gint column)
{
  return clist->column[column].justification;
}

gboolean
gtk_clist_column_visible_p (GtkCList *clist,
			   gint column)
{
  return clist->column[column].visible;
}

gboolean
gtk_clist_column_resizeable_p (GtkCList *clist,
			     gint column)
{
  return clist->column[column].resizeable;
}

gboolean
gtk_clist_column_auto_resize_p (GtkCList *clist,
				gint column)
{
  return clist->column[column].auto_resize;
}

gint
gtk_clist_column_width (GtkCList *clist,
			gint column)
{
  return clist->column[column].width;
}

gboolean
gtk_clist_auto_sort_p (GtkCList *clist)
{
  return GTK_CLIST_AUTO_SORT (clist);
}
#endif

/* CTree */

#ifdef CTREE
gboolean
gtk_ctree_node_leaf_p (GtkCTreeNode* node)
{
  return GTK_CTREE_ROW (node)->is_leaf;
}

GtkCTreeNode*
gtk_ctree_node_child (GtkCTreeNode* node)
{
  return GTK_CTREE_ROW (node)->children;
}

GtkCTreeNode*
gtk_ctree_node_parent (GtkCTreeNode* node)
{
  return GTK_CTREE_ROW (node)->parent;
}

GtkCTreeNode*
gtk_ctree_node_sibling (GtkCTreeNode* node)
{
  return GTK_CTREE_ROW (node)->sibling;
}

gint
gtk_ctree_node_level (GtkCTreeNode* node)
{
  return GTK_CTREE_ROW (node)->level;
}
#endif

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

gint
gtk_layout_get_size (GtkLayout *layout, gint *width, gint *height)
		 
{
  *width =  layout->width;
  *height = layout->height;
}

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


/* Menu */

gboolean
gtk_menu_get_tearoff_state (GtkMenu *menu)
{
  return menu->torn_off;
}

gchar*
gtk_menu_get_title (GtkMenu *menu)
{
  return g_strdup (gtk_object_get_data (GTK_OBJECT (menu), "gtk-menu-title"));
}


/* Table */

guint
gtk_table_row_spacing (GtkTable *table,
		       guint row)
{
  return table->rows[row].spacing;
}

guint
gtk_table_column_spacing (GtkTable *table,
			  guint col)
{
  return table->cols[col].spacing;
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


/* Tree */

GtkSelectionMode
gtk_tree_get_selection_mode (GtkTree *tree)
{
  return tree->selection_mode;
}

GtkSelectionMode
gtk_tree_get_view_mode (GtkTree *tree)
{
  return tree->view_mode;
}

GtkSelectionMode
gtk_tree_get_view_lines (GtkTree *tree)
{
  return tree->view_mode;
}

GtkTree*
gtk_tree_get_root_tree (GtkTree *tree)
{
  return GTK_TREE_ROOT_TREE (tree);
}

GList*
gtk_tree_selection (GtkTree *tree)
{
  return GTK_TREE_SELECTION (tree);
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


/* Scrolled window */
     
GtkWidget*
gtk_scrolled_window_get_hscrollbar (GtkScrolledWindow *window)
{
  return window->hscrollbar;
}

GtkWidget*
gtk_scrolled_window_get_vscrollbar (GtkScrolledWindow *window)
{
  return window->vscrollbar;
}



/* Tooltips */

guint
gtk_tooltips_get_delay (GtkTooltips *tooltips)
{
  return tooltips->delay;
}

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
