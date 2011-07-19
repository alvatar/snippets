
module dcairo.cairo_types;

version( cairo_1_8 ) { version = cairo_1_6; }
version( cairo_1_6 ) { version = cairo_1_4; }
version( cairo_1_4 ) { version = cairo_1_2; }
version( cairo_1_2 ) { version = cairo_1_0; }

extern(C):

enum cairo_antialias_t
{
    CAIRO_ANTIALIAS_DEFAULT,
    CAIRO_ANTIALIAS_NONE,
    CAIRO_ANTIALIAS_GRAY,
    CAIRO_ANTIALIAS_SUBPIXEL
}

alias int cairo_bool_t;

enum cairo_content_t
{
    CAIRO_CONTENT_COLOR = 0x1000,
    CAIRO_CONTENT_ALPHA = 0x2000,
    CAIRO_CONTENT_COLOR_ALPHA = 0x3000
}

alias void function(void* data) cairo_destroy_func_t;

enum cairo_extend_t
{
    CAIRO_EXTEND_NONE,
    CAIRO_EXTEND_REPEAT,
    CAIRO_EXTEND_REFLECT
}

enum cairo_fill_rule_t
{
    CAIRO_FILL_RULE_WINDING,
    CAIRO_FILL_RULE_EVEN_ODD
}

enum cairo_filter_t
{
    CAIRO_FILTER_FAST,
    CAIRO_FILTER_GOOD,
    CAIRO_FILTER_BEST,
    CAIRO_FILTER_NEAREST,
    CAIRO_FILTER_BILINEAR,
    CAIRO_FILTER_GAUSSIAN
}

enum cairo_font_slant_t
{
    CAIRO_FONT_SLANT_NORMAL,
    CAIRO_FONT_SLANT_ITALIC,
    CAIRO_FONT_SLANT_OBLIQUE
}

enum cairo_font_type_t
{
    CAIRO_FONT_TYPE_TOY,
    CAIRO_FONT_TYPE_FT,
    CAIRO_FONT_TYPE_WIN32,
    CAIRO_FONT_TYPE_QUARTZ,
    CAIRO_FONT_TYPE_USER
}

enum cairo_font_weight_t
{
    CAIRO_FONT_WEIGHT_NORMAL,
    CAIRO_FONT_WEIGHT_BOLD
}

enum cairo_format_t
{
    CAIRO_FORMAT_ARGB32,
    CAIRO_FORMAT_RGB24,
    CAIRO_FORMAT_A8,
    CAIRO_FORMAT_A1
}

enum cairo_hint_metrics_t
{
    CAIRO_HINT_METRICS_DEFAULT,
    CAIRO_HINT_METRICS_OFF,
    CAIRO_HINT_METRICS_ON
}

enum cairo_hint_style_t
{
    CAIRO_HINT_STYLE_DEFAULT,
    CAIRO_HINT_STYLE_NONE,
    CAIRO_HINT_STYLE_SLIGHT,
    CAIRO_HINT_STYLE_MEDIUM,
    CAIRO_HINT_STYLE_FULL
}

enum cairo_line_cap_t
{
    CAIRO_LINE_CAP_BUTT,
    CAIRO_LINE_CAP_ROUND,
    CAIRO_LINE_CAP_SQUARE
}

enum cairo_line_join_t
{
    CAIRO_LINE_JOIN_MITER,
    CAIRO_LINE_JOIN_ROUND,
    CAIRO_LINE_JOIN_BEVEL
}

enum cairo_operator_t
{
    CAIRO_OPERATOR_CLEAR,

    CAIRO_OPERATOR_SOURCE,
    CAIRO_OPERATOR_OVER,
    CAIRO_OPERATOR_IN,
    CAIRO_OPERATOR_OUT,
    CAIRO_OPERATOR_ATOP,

    CAIRO_OPERATOR_DEST,
    CAIRO_OPERATOR_DEST_OVER,
    CAIRO_OPERATOR_DEST_IN,
    CAIRO_OPERATOR_DEST_OUT,
    CAIRO_OPERATOR_DEST_ATOP,

    CAIRO_OPERATOR_XOR,
    CAIRO_OPERATOR_ADD,
    CAIRO_OPERATOR_SATURATE
}

enum cairo_path_data_type_t
{
    CAIRO_PATH_MOVE_TO,
    CAIRO_PATH_LINE_TO,
    CAIRO_PATH_CURVE_TO,
    CAIRO_PATH_CLOSE_PATH
}

enum cairo_pattern_type_t
{
    CAIRO_PATTERN_TYPE_SOLID,
    CAIRO_PATTERN_TYPE_SURFACE,
    CAIRO_PATTERN_TYPE_LINEAR,
    CAIRO_PATTERN_TYPE_RADIAL
}

enum cairo_status_t
{
    CAIRO_STATUS_SUCCESS = 0,
    CAIRO_STATUS_NO_MEMORY,
    CAIRO_STATUS_INVALID_RESTORE,
    CAIRO_STATUS_INVALID_POP_GROUP,
    CAIRO_STATUS_NO_CURRENT_POINT,
    CAIRO_STATUS_INVALID_MATRIX,
    CAIRO_STATUS_INVALID_STATUS,
    CAIRO_STATUS_NULL_POINTER,
    CAIRO_STATUS_INVALID_STRING,
    CAIRO_STATUS_INVALID_PATH_DATA,
    CAIRO_STATUS_READ_ERROR,
    CAIRO_STATUS_WRITE_ERROR,
    CAIRO_STATUS_SURFACE_FINISHED,
    CAIRO_STATUS_SURFACE_TYPE_MISMATCH,
    CAIRO_STATUS_PATTERN_TYPE_MISMATCH,
    CAIRO_STATUS_INVALID_CONTENT,
    CAIRO_STATUS_INVALID_FORMAT,
    CAIRO_STATUS_INVALID_VISUAL,
    CAIRO_STATUS_FILE_NOT_FOUND,
    CAIRO_STATUS_INVALID_DASH,
    CAIRO_STATUS_INVALID_DSC_COMMENT,
    CAIRO_STATUS_INVALID_INDEX,
    CAIRO_STATUS_CLIP_NOT_REPRESENTABLE,
    CAIRO_STATUS_TEMP_FILE_ERROR,
    CAIRO_STATUS_INVALID_STRIDE,
    CAIRO_STATUS_FONT_TYPE_MISMATCH,
    CAIRO_STATUS_USER_FONT_IMMUTABLE,
    CAIRO_STATUS_USER_FONT_ERROR,
    CAIRO_STATUS_NEGATIVE_COUNT,
    CAIRO_STATUS_INVALID_CLUSTERS,
    CAIRO_STATUS_INVALID_SLANT,
    CAIRO_STATUS_INVALID_WEIGHT
}

enum cairo_subpixel_order_t
{
    CAIRO_SUBPIXEL_ORDER_DEFAULT,
    CAIRO_SUBPIXEL_ORDER_RGB,
    CAIRO_SUBPIXEL_ORDER_BGR,
    CAIRO_SUBPIXEL_ORDER_VRGB,
    CAIRO_SUBPIXEL_ORDER_VBGR
}

enum cairo_surface_type_t
{
    CAIRO_SURFACE_TYPE_IMAGE,
    CAIRO_SURFACE_TYPE_PDF,
    CAIRO_SURFACE_TYPE_PS,
    CAIRO_SURFACE_TYPE_XLIB,
    CAIRO_SURFACE_TYPE_XCB,
    CAIRO_SURFACE_TYPE_GLITZ,
    CAIRO_SURFACE_TYPE_QUARTZ,
    CAIRO_SURFACE_TYPE_WIN32,
    CAIRO_SURFACE_TYPE_BEOS,
    CAIRO_SURFACE_TYPE_DIRECTFB,
    CAIRO_SURFACE_TYPE_SVG,
    CAIRO_SURFACE_TYPE_OS2,
    CAIRO_SURFACE_TYPE_WIN32_PRINTING,
    CAIRO_SURFACE_TYPE_QUARTZ_IMAGE
}

enum cairo_text_cluster_flags_t
{
    CAIRO_TEXT_CLUSTER_FLAG_BACKWARD = 0x00000001
}

struct cairo_font_extents_t
{
    double ascent;
    double descent;
    double height;
    double max_x_advance;
    double max_y_advance;
}

struct cairo_font_face_t {}

struct cairo_font_options_t {}

struct cairo_glyph_t
{
    ulong index;
    double x;
    double y;
}

struct cairo_matrix_t
{
    double xx; double yx;
    double xy; double yy;
    double x0; double y0;
}

union cairo_path_data_t
{
    struct
    {
        cairo_path_data_type_t type;
        int length = 0;
    }
    struct
    {
        double x, y;
    }
}

struct cairo_path_t
{
    cairo_status_t status = cairo_status_t.CAIRO_STATUS_SUCCESS;
    cairo_path_data_t *data = null;
    int num_data = 0;
}

struct cairo_pattern_t {}

alias cairo_status_t function(void* closure, ubyte* data, uint length) cairo_read_func_t;

struct cairo_rectangle_list_t
{
    cairo_status_t status;
    cairo_rectangle_t* rectangles;
    int num_rectangles;
}

struct cairo_rectangle_t
{
    double x, y, width, height;
}

struct cairo_scaled_font_t {}

struct cairo_surface_t {}

struct cairo_t {}

struct cairo_text_cluster_t
{
    int num_bytes;
    int num_glyphs;
}

struct cairo_text_extents_t
{
    double x_bearing;
    double y_bearing;
    double width;
    double height;
    double x_advance;
    double y_advance;
}

struct cairo_user_data_key_t
{
    int unused;
}

alias cairo_status_t function(void* closure, ubyte* data, uint length) cairo_write_func_t;

