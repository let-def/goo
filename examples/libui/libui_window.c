#include "libui_window.h"
#define WIDGET uiWindow($field(self, control))

static void on_content_size_changed(uiWindow *w, void *self)
{
  $static(event, content_size_changed)((libui_window*)self);
}

static int on_closing(uiWindow *w, void *self)
{
  return $static(event, closing)((libui_window*)self);
}

$method libui_window *self_new(goo_string title, int width, int height, goo_bool has_menubar)
{
  libui_window *self = $alloc();
  $field(self, control) = uiControl(uiNewWindow(goo_string_data(title), width, height, has_menubar));
  uiWindowOnContentSizeChanged(WIDGET, on_content_size_changed, self);
  uiWindowOnClosing(WIDGET, on_closing, self);
  return self;
}

$method goo_bool self_is_margined(libui_window *self)
{
  return uiWindowMargined(WIDGET);
}

$method void self_set_margined(libui_window *self, goo_bool margined)
{
  uiWindowSetMargined(WIDGET, margined);
}

$method goo_bool self_is_borderless(libui_window *self)
{
  return uiWindowBorderless(WIDGET);
}

$method void self_set_borderless(libui_window *self, goo_bool borderless)
{
  uiWindowSetBorderless(WIDGET, borderless);
}

$method goo_bool self_is_fullscreen(libui_window *self)
{
  return uiWindowFullscreen(WIDGET);
}

$method void self_set_fullscreen(libui_window *self, goo_bool fullscreen)
{
  uiWindowSetFullscreen(WIDGET, fullscreen);
}

$method void self_set_content_size(libui_window *self, int width, int height)
{
  uiWindowSetContentSize(WIDGET, width, height);
}

$method void self_content_size(libui_window *self, int *ret0, int *ret1)
{
  uiWindowContentSize(WIDGET, ret0, ret1);
}

$method void self_set_title(libui_window *self, goo_string title)
{
  uiWindowSetTitle(WIDGET, goo_string_data(title));
}

$method goo_string self_title(libui_window *self)
{
  return goo_string_from_c(uiWindowTitle(WIDGET));
}

$method void self_child_connect(libui_window *self, libui_control *child)
{
  $static(connect, child)(self, child);
  uiWindowSetChild(WIDGET, $field($field(self, child), control));
}

$method void self_on_child_disconnect(libui_window *self, libui_control *object)
{
  uiWindowSetChild(WIDGET, NULL);
}

