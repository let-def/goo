#include "libui_window.h"
#define WIDGET uiWindow($field(self, control))

static void on_content_size_changed(uiWindow *w, void *self)
{
  $static(self, on_content_size_changed)((libui_window*)self);
}

static int on_closing(uiWindow *w, void *self)
{
  return $static(self, on_closing)((libui_window*)self);
}

libui_window * libui_window_new(goo_string title, int width, int height, goo_bool has_menubar)
{
  libui_window *self = $alloc();
  $field(self, control) = uiControl(uiNewWindow(goo_string_data(title), width, height, has_menubar));
  uiWindowOnContentSizeChanged(WIDGET, on_content_size_changed, self);
  uiWindowOnClosing(WIDGET, on_closing, self);
  return self;
}

goo_bool libui_window_is_margined(libui_window *self)
{
  return uiWindowMargined(WIDGET);
}

void libui_window_set_margined(libui_window *self, goo_bool margined)
{
  uiWindowSetMargined(WIDGET, margined);
}

goo_bool libui_window_is_borderless(libui_window *self)
{
  return uiWindowBorderless(WIDGET);
}

void libui_window_set_borderless(libui_window *self, goo_bool borderless)
{
  uiWindowSetBorderless(WIDGET, borderless);
}

goo_bool libui_window_is_fullscreen(libui_window *self)
{
  return uiWindowFullscreen(WIDGET);
}

void libui_window_set_fullscreen(libui_window *self, goo_bool fullscreen)
{
  uiWindowSetFullscreen(WIDGET, fullscreen);
}

void libui_window_set_content_size(libui_window *self, int width, int height)
{
  uiWindowSetContentSize(WIDGET, width, height);
}


int libui_window_content_height(libui_window *self)
{
  int width, height;
  uiWindowContentSize(WIDGET, &width, &height);
  return height;
}

int libui_window_content_width(libui_window *self)
{
  int width, height;
  uiWindowContentSize(WIDGET, &width, &height);
  return width;
}

void libui_window_set_title(libui_window *self, goo_string title)
{
  uiWindowSetTitle(WIDGET, goo_string_data(title));
}

goo_string libui_window_title(libui_window *self)
{
  return goo_string_from_c(uiWindowTitle(WIDGET), 0);
}

void libui_window_child_connect(libui_window *self, libui_control *child)
{
  $static(connect, child)(self, child);
  uiWindowSetChild(WIDGET, $field($field(self, child), control));
}

void libui_window_on_child_disconnect(libui_window *self, libui_control *object)
{
  uiWindowSetChild(WIDGET, NULL);
}

