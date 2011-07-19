
module dcairo.cairo_functions;

/* This is to asume older versions when new ones appear */
version( cairo_2_0 ) { version = cairo_1_9; }

private
{
    import dcairo.cairo_types;
    import dloader.library_loader;
}

/**
 * This function encodes the given cairo version into an integer. The numbers
 * returned by cairo_version() are encoded using this function. Two encoded
 * version numbers can be compared as integers. The encoding ensures that
 * later versions compare greater than earlier versions.
 */
int CAIRO_VERSION_ENCODE(int major, int minor, int micro)
{
    return major*10_000 + minor*100 + micro;
}


/*
TODO: NOT IMPLEMENTED

cairo_debug_reset_static_data ()
cairo_format_stride_for_width ()
cairo_ft_font_face_create_for_ft_face()
cairo_ft_font_face_create_for_pattern ()
cairo_ft_font_options_substitute ()
cairo_ft_scaled_font_lock_face ()
cairo_ft_scaled_font_unlock_face ()
cairo_glyph_allocate ()
cairo_has_current_point ()
cairo_image_surface_create_from_png ()
cairo_image_surface_create_from_png_stream ()
cairo_path_extents ()
cairo_pdf_surface_create ()
cairo_pdf_surface_create_for_stream ()
cairo_pdf_surface_set_size ()
cairo_ps_get_levels ()
enum cairo_ps_level_t
cairo_ps_level_to_string ()
cairo_ps_surface_create ()
cairo_ps_surface_create_for_stream ()
cairo_ps_surface_dsc_begin_page_setup ()
cairo_ps_surface_dsc_begin_setup ()
cairo_ps_surface_dsc_comment ()
cairo_ps_surface_get_eps ()
cairo_ps_surface_restrict_to_level ()
cairo_ps_surface_set_eps ()
cairo_ps_surface_set_size ()
cairo_quartz_font_face_create_for_atsu_font_id ()
cairo_quartz_font_face_create_for_cgfont ()
cairo_quartz_surface_create ()
cairo_quartz_surface_create_for_cg_context ()
cairo_quartz_surface_get_cg_context ()
cairo_scaled_font_get_scale_matrix ()
cairo_scaled_font_text_to_glyphs ()
cairo_show_text_glyphs ()
cairo_surface_copy_page ()
cairo_surface_get_fallback_resolution ()
cairo_surface_has_show_text_glyphs ()
cairo_surface_show_page ()
cairo_surface_write_to_png ()
cairo_surface_write_to_png_stream ()
cairo_svg_get_versions ()
cairo_svg_surface_create ()
cairo_svg_surface_create_for_stream ()
cairo_svg_surface_restrict_to_version ()
cairo_svg_version_to_string ()
cairo_text_cluster_allocate ()
cairo_text_cluster_free()
cairo_toy_font_face_create ()
cairo_toy_font_face_get_family ()
cairo_toy_font_face_get_slant ()
cairo_toy_font_face_get_weight ()
cairo_user_font_face_create ()
cairo_user_font_face_get_init_func ()
cairo_user_font_face_get_render_glyph_func ()
cairo_user_font_face_get_text_to_glyphs_func ()
cairo_user_font_face_get_unicode_to_glyph_func ()
cairo_user_font_face_set_init_func ()
cairo_user_font_face_set_render_glyph_func ()
cairo_user_font_face_set_text_to_glyphs_func ()
cairo_user_font_face_set_unicode_to_glyph_func ()
cairo_user_scaled_font_init_func_t ()
cairo_user_scaled_font_render_glyph_func_t ()
cairo_user_scaled_font_text_to_glyphs_func_t ()
cairo_user_scaled_font_unicode_to_glyph_func_t ()
cairo_win32_font_face_create_for_hfont ()
cairo_win32_font_face_create_for_logfontw ()
cairo_win32_font_face_create_for_logfontw_hfont ()
cairo_win32_printing_surface_create ()
cairo_win32_scaled_font_done_font ()
cairo_win32_scaled_font_get_device_to_logical ()
cairo_win32_scaled_font_get_logical_to_device ()
cairo_win32_scaled_font_get_metrics_factor ()
cairo_win32_scaled_font_select_font ()
cairo_win32_surface_create ()
cairo_win32_surface_create_with_ddb ()
cairo_win32_surface_create_with_dib ()
cairo_win32_surface_get_dc ()
cairo_win32_surface_get_image ()
cairo_xlib_surface_create ()
cairo_xlib_surface_create_for_bitmap ()
cairo_xlib_surface_get_depth ()
cairo_xlib_surface_get_display ()
cairo_xlib_surface_get_drawable ()
cairo_xlib_surface_get_height ()
cairo_xlib_surface_get_screen ()
cairo_xlib_surface_get_visual ()
cairo_xlib_surface_get_width ()
cairo_xlib_surface_set_drawable ()
cairo_xlib_surface_set_size () 
 */



// C calling convention for BOTH linux and Windows
extern(C):

// typedef Tr function( Ta... ) pfName;
// pfName Name;
typedef void function(cairo_t* cr, cairo_path_t* path) pf_cairo_append_path;
pf_cairo_append_path cairo_append_path;
typedef void function(cairo_t* cr, double xc, double yc, double radius, double angle1, double angle2) pf_cairo_arc;
pf_cairo_arc cairo_arc;
typedef void function(cairo_t* cr, double xc, double yc, double radius, double angle1, double angle2) pf_cairo_arc_negative;
pf_cairo_arc_negative cairo_arc_negative;
typedef void function(cairo_t* cr) pf_cairo_clip;
pf_cairo_clip cairo_clip;
typedef void function(cairo_t* cr, double* x1, double* y1, double* x2, double* y2) pf_cairo_clip_extents;
pf_cairo_clip_extents cairo_clip_extents;
typedef void function(cairo_t* cr) pf_cairo_clip_preserve;
pf_cairo_clip_preserve cairo_clip_preserve;
typedef void function(cairo_t* cr) pf_cairo_close_path;
pf_cairo_close_path cairo_close_path;
typedef cairo_rectangle_list_t* function(cairo_t* cr) pf_cairo_copy_clip_rectangle_list;
pf_cairo_copy_clip_rectangle_list cairo_copy_clip_rectangle_list;
typedef void function(cairo_t* cr) pf_cairo_copy_page;
pf_cairo_copy_page cairo_copy_page;
typedef cairo_path_t* function(cairo_t* cr) pf_cairo_copy_path;
pf_cairo_copy_path cairo_copy_path;
typedef cairo_path_t* function(cairo_t* cr) pf_cairo_copy_path_flat;
pf_cairo_copy_path_flat cairo_copy_path_flat;
typedef cairo_t* function(cairo_surface_t* target) pf_cairo_create;
pf_cairo_create cairo_create;
typedef void function(cairo_t* cr, double x1, double y1, double x2, double y2, double x3, double y3) pf_cairo_curve_to;
pf_cairo_curve_to cairo_curve_to;
typedef void function(cairo_t* cr) pf_cairo_destroy;
pf_cairo_destroy cairo_destroy;
typedef void function(cairo_t* cr, double* x, double* y) pf_cairo_device_to_user;
pf_cairo_device_to_user cairo_device_to_user;
typedef void function(cairo_t* cr, double* dx, double* dy) pf_cairo_device_to_user_distance;
pf_cairo_device_to_user_distance cairo_device_to_user_distance;
pf_cairo_fill cairo_fill;
typedef void function(cairo_t* cr) pf_cairo_fill;
typedef cairo_bool_t function(cairo_t* cr, double* x1, double* y1, double* x2, double* y2) pf_cairo_fill_extents;
pf_cairo_fill_extents cairo_fill_extents;
typedef void function(cairo_t* cr) pf_cairo_fill_preserve;
pf_cairo_fill_preserve cairo_fill_preserve;
typedef void function(cairo_t* cr, cairo_font_extents_t* extents) pf_cairo_font_extents;
pf_cairo_font_extents cairo_font_extents;
typedef void function(cairo_font_face_t* font_face) pf_cairo_font_face_destroy;
pf_cairo_font_face_destroy cairo_font_face_destroy;
typedef uint function(cairo_font_face_t* font_face) pf_cairo_font_face_get_reference_count;
pf_cairo_font_face_get_reference_count cairo_font_face_get_reference_count;
typedef cairo_font_type_t function(cairo_font_face_t* font_face) pf_cairo_font_face_get_type;
pf_cairo_font_face_get_type cairo_font_face_get_type;
typedef void* function(cairo_font_face_t* font_face, cairo_user_data_key_t* key) pf_cairo_font_face_get_user_data;
pf_cairo_font_face_get_user_data cairo_font_face_get_user_data;
typedef cairo_font_face_t* function(cairo_font_face_t* font_face) pf_cairo_font_face_reference;
pf_cairo_font_face_reference cairo_font_face_reference;
typedef cairo_status_t function(cairo_font_face_t* font_face, cairo_user_data_key_t* key, void* user_data, cairo_destroy_func_t destroy) pf_cairo_font_face_set_user_data;
pf_cairo_font_face_set_user_data cairo_font_face_set_user_data;
typedef cairo_status_t function(cairo_font_face_t* font_face) pf_cairo_font_face_status;
pf_cairo_font_face_status cairo_font_face_status;
typedef cairo_font_options_t* function(cairo_font_options_t* original) pf_cairo_font_options_copy;
pf_cairo_font_options_copy cairo_font_options_copy;
typedef cairo_font_options_t* function() pf_cairo_font_options_create;
pf_cairo_font_options_create cairo_font_options_create;
typedef void function(cairo_font_options_t* options) pf_cairo_font_options_destroy;
pf_cairo_font_options_destroy cairo_font_options_destroy;
typedef cairo_bool_t function(cairo_font_options_t* options, cairo_font_options_t* other) pf_cairo_font_options_equal;
pf_cairo_font_options_equal cairo_font_options_equal;
typedef cairo_antialias_t function(cairo_font_options_t* options) pf_cairo_font_options_get_antialias;
pf_cairo_font_options_get_antialias cairo_font_options_get_antialias;
typedef cairo_hint_metrics_t function(cairo_font_options_t* options) pf_cairo_font_options_get_hint_metrics;
pf_cairo_font_options_get_hint_metrics cairo_font_options_get_hint_metrics;
typedef cairo_hint_style_t function(cairo_font_options_t* options) pf_cairo_font_options_get_hint_style;
pf_cairo_font_options_get_hint_style cairo_font_options_get_hint_style;
typedef cairo_subpixel_order_t function(cairo_font_options_t* options) pf_cairo_font_options_get_subpixel_order;
pf_cairo_font_options_get_subpixel_order cairo_font_options_get_subpixel_order;
typedef uint function(cairo_font_options_t* options) pf_cairo_font_options_hash;
pf_cairo_font_options_hash cairo_font_options_hash;
typedef void function(cairo_font_options_t* options, cairo_font_options_t* other) pf_cairo_font_options_merge;
pf_cairo_font_options_merge cairo_font_options_merge;
typedef void function(cairo_font_options_t* options, cairo_antialias_t antialias) pf_cairo_font_options_set_antialias;
pf_cairo_font_options_set_antialias cairo_font_options_set_antialias;
typedef void function(cairo_font_options_t* options, cairo_hint_metrics_t hint_metrics) pf_cairo_font_options_set_hint_metrics;
pf_cairo_font_options_set_hint_metrics cairo_font_options_set_hint_metrics;
typedef void function(cairo_font_options_t* options, cairo_hint_style_t hint_style) pf_cairo_font_options_set_hint_style;
pf_cairo_font_options_set_hint_style cairo_font_options_set_hint_style;
typedef void function(cairo_font_options_t* options, cairo_subpixel_order_t subpixel_order) pf_cairo_font_options_set_subpixel_order;
pf_cairo_font_options_set_subpixel_order cairo_font_options_set_subpixel_order;
typedef cairo_status_t function(cairo_font_options_t* options) pf_cairo_font_options_status;
pf_cairo_font_options_status cairo_font_options_status;
typedef cairo_antialias_t function(cairo_t* cr) pf_cairo_get_antialias;
pf_cairo_get_antialias cairo_get_antialias;
typedef void function(cairo_t* cr, double* x, double* y) pf_cairo_get_current_point;
pf_cairo_get_current_point cairo_get_current_point;
typedef void function(cairo_t* cr, double* dashes, double* offset) pf_cairo_get_dash;
pf_cairo_get_dash cairo_get_dash;
typedef int function(cairo_t* cr) pf_cairo_get_dash_count;
pf_cairo_get_dash_count cairo_get_dash_count;
typedef cairo_fill_rule_t function(cairo_t* cr) pf_cairo_get_fill_rule;
pf_cairo_get_fill_rule cairo_get_fill_rule;
typedef cairo_font_face_t* function(cairo_t* cr) pf_cairo_get_font_face;
pf_cairo_get_font_face cairo_get_font_face;
typedef void function(cairo_t* cr, cairo_matrix_t* matrix) pf_cairo_get_font_matrix;
pf_cairo_get_font_matrix cairo_get_font_matrix;
typedef void function(cairo_t* cr, cairo_font_options_t* options) pf_cairo_get_font_options;
pf_cairo_get_font_options cairo_get_font_options;
typedef cairo_surface_t* function(cairo_t* cr) pf_cairo_get_group_target;
pf_cairo_get_group_target cairo_get_group_target;
typedef cairo_line_cap_t function(cairo_t* cr) pf_cairo_get_line_cap;
pf_cairo_get_line_cap cairo_get_line_cap;
typedef cairo_line_join_t function(cairo_t* cr) pf_cairo_get_line_join;
pf_cairo_get_line_join cairo_get_line_join;
typedef double function(cairo_t* cr) pf_cairo_get_line_width;
pf_cairo_get_line_width cairo_get_line_width;
typedef double function(cairo_t* cr) pf_cairo_get_miter_limit;
pf_cairo_get_miter_limit cairo_get_miter_limit;
typedef cairo_operator_t function(cairo_t* cr) pf_cairo_get_operator;
pf_cairo_get_operator cairo_get_operator;
typedef uint function(cairo_t* cr) pf_cairo_get_reference_count;
pf_cairo_get_reference_count cairo_get_reference_count;
typedef void function(cairo_t* cr, cairo_matrix_t* matrix) pf_cairo_get_matrix;
pf_cairo_get_matrix cairo_get_matrix;
typedef cairo_scaled_font_t* function(cairo_t* cr) pf_cairo_get_scaled_font;
pf_cairo_get_scaled_font cairo_get_scaled_font;
typedef cairo_pattern_t* function(cairo_t* cr) pf_cairo_get_source;
pf_cairo_get_source cairo_get_source;
typedef cairo_surface_t* function(cairo_t* cr) pf_cairo_get_target;
pf_cairo_get_target cairo_get_target;
typedef double function(cairo_t* cr) pf_cairo_get_tolerance;
pf_cairo_get_tolerance cairo_get_tolerance;
typedef void* function(cairo_t* cr, cairo_user_data_key_t* key) pf_cairo_get_user_data;
pf_cairo_get_user_data cairo_get_user_data;
typedef void function(cairo_t* cr, cairo_glyph_t* glyphs, int num_glyphs, cairo_text_extents_t* extents) pf_cairo_glyph_extents;
pf_cairo_glyph_extents cairo_glyph_extents;
typedef void function(cairo_t* cr, cairo_glyph_t* glyphs, int num_glyphs) pf_cairo_glyph_path;
pf_cairo_glyph_path cairo_glyph_path;
typedef void function(cairo_t* cr) pf_cairo_identity_matrix;
pf_cairo_identity_matrix cairo_identity_matrix;
typedef cairo_surface_t* function(cairo_format_t format, int width, int height) pf_cairo_image_surface_create;
pf_cairo_image_surface_create cairo_image_surface_create;
typedef cairo_surface_t* function(ubyte* data, cairo_format_t format, int width, int height, int stride) pf_cairo_image_surface_create_for_data;
pf_cairo_image_surface_create_for_data cairo_image_surface_create_for_data;
typedef ubyte* function(cairo_surface_t* surface) pf_cairo_image_surface_get_data;
pf_cairo_image_surface_get_data cairo_image_surface_get_data;
typedef cairo_format_t function(cairo_surface_t* surface) pf_cairo_image_surface_get_format;
pf_cairo_image_surface_get_format cairo_image_surface_get_format;
typedef int function(cairo_surface_t* surface) pf_cairo_image_surface_get_height;
pf_cairo_image_surface_get_height cairo_image_surface_get_height;
typedef int function(cairo_surface_t* surface) pf_cairo_image_surface_get_stride;
pf_cairo_image_surface_get_stride cairo_image_surface_get_stride;
typedef int function(cairo_surface_t* surface) pf_cairo_image_surface_get_width;
pf_cairo_image_surface_get_width cairo_image_surface_get_width;
typedef cairo_bool_t function(cairo_t* cr, double x, double y) pf_cairo_in_fill;
pf_cairo_in_fill cairo_in_fill;
typedef cairo_bool_t function(cairo_t* cr, double x, double y) pf_cairo_in_stroke;
pf_cairo_in_stroke cairo_in_stroke;
typedef void function(cairo_t* cr, double x, double y) pf_cairo_line_to;
pf_cairo_line_to cairo_line_to;
typedef void function(cairo_t* cr, cairo_pattern_t* pattern) pf_cairo_mask;
pf_cairo_mask cairo_mask;
typedef void function(cairo_t* cr, cairo_surface_t* surface, double surface_x, double surface_y) pf_cairo_mask_surface;
pf_cairo_mask_surface cairo_mask_surface;
typedef void function(cairo_matrix_t* matrix, double xx, double yx, double xy, double yy, double x0, double y0) pf_cairo_matrix_init;
pf_cairo_matrix_init cairo_matrix_init;
typedef void function(cairo_matrix_t* matrix) pf_cairo_matrix_init_identity;
pf_cairo_matrix_init_identity cairo_matrix_init_identity;
typedef void function(cairo_matrix_t* matrix, double radians) pf_cairo_matrix_init_rotate;
pf_cairo_matrix_init_rotate cairo_matrix_init_rotate;
typedef void function(cairo_matrix_t* matrix, double sx, double sy) pf_cairo_matrix_init_scale;
pf_cairo_matrix_init_scale cairo_matrix_init_scale;
typedef void function(cairo_matrix_t* matrix, double tx, double ty) pf_cairo_matrix_init_translate;
pf_cairo_matrix_init_translate cairo_matrix_init_translate;
typedef cairo_status_t function(cairo_matrix_t* matrix) pf_cairo_matrix_invert;
pf_cairo_matrix_invert cairo_matrix_invert;
typedef void function(cairo_matrix_t* result, cairo_matrix_t* a, cairo_matrix_t* b) pf_cairo_matrix_multiply;
pf_cairo_matrix_multiply cairo_matrix_multiply;
typedef void function(cairo_matrix_t* matrix, double radians) pf_cairo_matrix_rotate;
pf_cairo_matrix_rotate cairo_matrix_rotate;
typedef void function(cairo_matrix_t* matrix, double sx, double sy) pf_cairo_matrix_scale;
pf_cairo_matrix_scale cairo_matrix_scale;
typedef void function(cairo_matrix_t* matrix, double* dx, double* dy) pf_cairo_matrix_transform_distance;
pf_cairo_matrix_transform_distance cairo_matrix_transform_distance;
typedef void function(cairo_matrix_t* matrix, double* x, double* y) pf_cairo_matrix_transform_point;
pf_cairo_matrix_transform_point cairo_matrix_transform_point;
typedef void function(cairo_matrix_t* matrix, double tx, double ty) pf_cairo_matrix_translate;
pf_cairo_matrix_translate cairo_matrix_translate;
typedef void function(cairo_t* cr, double x, double y) pf_cairo_move_to;
pf_cairo_move_to cairo_move_to;
typedef void function(cairo_t* cr) pf_cairo_new_path;
pf_cairo_new_path cairo_new_path;
typedef void function(cairo_t* cr) pf_cairo_new_sub_path;
pf_cairo_new_sub_path cairo_new_sub_path;
typedef void function(cairo_t* cr) pf_cairo_paint;
pf_cairo_paint cairo_paint;
typedef void function(cairo_t* cr, double alpha) pf_cairo_paint_with_alpha;
pf_cairo_paint_with_alpha cairo_paint_with_alpha;
typedef void function(cairo_path_t* path) pf_cairo_path_destroy;
pf_cairo_path_destroy cairo_path_destroy;
typedef void function(cairo_pattern_t* pattern, double offset, double red, double green, double blue) pf_cairo_pattern_add_color_stop_rgb;
pf_cairo_pattern_add_color_stop_rgb cairo_pattern_add_color_stop_rgb;
typedef void function(cairo_pattern_t* pattern, double offset, double red, double green, double blue, double alpha) pf_cairo_pattern_add_color_stop_rgba;
pf_cairo_pattern_add_color_stop_rgba cairo_pattern_add_color_stop_rgba;
typedef cairo_pattern_t* function(cairo_surface_t* surface) pf_cairo_pattern_create_for_surface;
pf_cairo_pattern_create_for_surface cairo_pattern_create_for_surface;
typedef cairo_pattern_t* function(double x0, double y0, double x1, double y1) pf_cairo_pattern_create_linear;
pf_cairo_pattern_create_linear cairo_pattern_create_linear;
typedef cairo_pattern_t* function(double cx0, double cy0, double radius0, double cx1, double cy1, double radius1) pf_cairo_pattern_create_radial;
pf_cairo_pattern_create_radial cairo_pattern_create_radial;
typedef cairo_pattern_t* function(double red, double green, double blue) pf_cairo_pattern_create_rgb;
pf_cairo_pattern_create_rgb cairo_pattern_create_rgb;
typedef cairo_pattern_t* function(double red, double green, double blue, double alpha) pf_cairo_pattern_create_rgba;
pf_cairo_pattern_create_rgba cairo_pattern_create_rgba;
typedef void function(cairo_pattern_t* pattern) pf_cairo_pattern_destroy;
pf_cairo_pattern_destroy cairo_pattern_destroy;
typedef cairo_status_t function(cairo_pattern_t* pattern, int* count) pf_cairo_pattern_get_color_stop_count;
pf_cairo_pattern_get_color_stop_count cairo_pattern_get_color_stop_count;
typedef cairo_status_t function(cairo_pattern_t* pattern, int index, double* offset, double* red, double* green, double* blue, double* alpha) pf_cairo_pattern_get_color_stop_rgba;
pf_cairo_pattern_get_color_stop_rgba cairo_pattern_get_color_stop_rgba;
typedef cairo_extend_t function(cairo_pattern_t* pattern) pf_cairo_pattern_get_extend;
pf_cairo_pattern_get_extend cairo_pattern_get_extend;
typedef cairo_filter_t function(cairo_pattern_t* pattern) pf_cairo_pattern_get_filter;
pf_cairo_pattern_get_filter cairo_pattern_get_filter;
typedef cairo_status_t function(cairo_pattern_t* pattern, double* x0, double* y0, double* x1, double* y1) pf_cairo_pattern_get_linear_points;
pf_cairo_pattern_get_linear_points cairo_pattern_get_linear_points;
typedef void function(cairo_pattern_t* pattern, cairo_matrix_t* matrix) pf_cairo_pattern_get_matrix;
pf_cairo_pattern_get_matrix cairo_pattern_get_matrix;
typedef cairo_status_t function(cairo_pattern_t* pattern, double* x0, double* y0, double* r0, double* x1, double* y1, double* r1) pf_cairo_pattern_get_radial_circles;
pf_cairo_pattern_get_radial_circles cairo_pattern_get_radial_circles;
typedef uint function(cairo_pattern_t* pattern) pf_cairo_pattern_get_reference_count;
pf_cairo_pattern_get_reference_count cairo_pattern_get_reference_count;
typedef cairo_status_t function(cairo_pattern_t* pattern, double* red, double* green, double* blue, double* alpha) pf_cairo_pattern_get_rgba;
pf_cairo_pattern_get_rgba cairo_pattern_get_rgba;
typedef cairo_status_t function(cairo_pattern_t* pattern, cairo_surface_t** surface) pf_cairo_pattern_get_surface;
pf_cairo_pattern_get_surface cairo_pattern_get_surface;
typedef cairo_pattern_type_t function(cairo_pattern_t* pattern) pf_cairo_pattern_get_type;
pf_cairo_pattern_get_type cairo_pattern_get_type;
typedef void* function(cairo_pattern_t* pattern, cairo_user_data_key_t* key) pf_cairo_pattern_get_user_data;
pf_cairo_pattern_get_user_data cairo_pattern_get_user_data;
typedef cairo_pattern_t* function(cairo_pattern_t* pattern) pf_cairo_pattern_reference;
pf_cairo_pattern_reference cairo_pattern_reference;
typedef void function(cairo_pattern_t* pattern, cairo_extend_t extend) pf_cairo_pattern_set_extend;
pf_cairo_pattern_set_extend cairo_pattern_set_extend;
typedef void function(cairo_pattern_t* pattern, cairo_filter_t filter) pf_cairo_pattern_set_filter;
pf_cairo_pattern_set_filter cairo_pattern_set_filter;
typedef void function(cairo_pattern_t* pattern, cairo_matrix_t* matrix) pf_cairo_pattern_set_matrix;
pf_cairo_pattern_set_matrix cairo_pattern_set_matrix;
typedef cairo_status_t function(cairo_pattern_t* pattern, cairo_user_data_key_t* key, void* user_data, cairo_destroy_func_t destroy) pf_cairo_pattern_set_user_data;
pf_cairo_pattern_set_user_data cairo_pattern_set_user_data;
typedef cairo_status_t function(cairo_pattern_t* pattern) pf_cairo_pattern_status;
pf_cairo_pattern_status cairo_pattern_status;
typedef cairo_pattern_t* function(cairo_t* cr) pf_cairo_pop_group;
pf_cairo_pop_group cairo_pop_group;
typedef void function(cairo_t* cr) pf_cairo_pop_group_to_source;
pf_cairo_pop_group_to_source cairo_pop_group_to_source;
typedef void function(cairo_t* cr) pf_cairo_push_group;
pf_cairo_push_group cairo_push_group;
typedef void function(cairo_t* cr, cairo_content_t content) pf_cairo_push_group_with_content;
pf_cairo_push_group_with_content cairo_push_group_with_content;
typedef void function(cairo_t* cr, double x, double y, double width, double height) pf_cairo_rectangle;
pf_cairo_rectangle cairo_rectangle;
typedef void function(cairo_rectangle_list_t* rectangle_list) pf_cairo_rectangle_list_destroy;
pf_cairo_rectangle_list_destroy cairo_rectangle_list_destroy;
typedef cairo_t* function(cairo_t* cr) pf_cairo_reference;
pf_cairo_reference cairo_reference;
typedef void function(cairo_t* cr, double dx1, double dy1, double dx2, double dy2, double dx3, double dy3) pf_cairo_rel_curve_to;
pf_cairo_rel_curve_to cairo_rel_curve_to;
typedef void function(cairo_t* cr, double dx, double dy) pf_cairo_rel_line_to;
pf_cairo_rel_line_to cairo_rel_line_to;
typedef void function(cairo_t* cr, double dx, double dy) pf_cairo_rel_move_to;
pf_cairo_rel_move_to cairo_rel_move_to;
typedef void function(cairo_t* cr) pf_cairo_reset_clip;
pf_cairo_reset_clip cairo_reset_clip;
typedef void function(cairo_t* cr) pf_cairo_restore;
pf_cairo_restore cairo_restore;
typedef void function(cairo_t* cr, double angle) pf_cairo_rotate;
pf_cairo_rotate cairo_rotate;
typedef void function(cairo_t* cr) pf_cairo_save;
pf_cairo_save cairo_save;
typedef void function(cairo_t* cr, double sx, double sy) pf_cairo_scale;
pf_cairo_scale cairo_scale;
typedef cairo_scaled_font_t* function(cairo_font_face_t* font_face, cairo_matrix_t* font_matrix, cairo_matrix_t* ctm, cairo_font_options_t* options) pf_cairo_scaled_font_create;
pf_cairo_scaled_font_create cairo_scaled_font_create;
typedef void function(cairo_scaled_font_t* scaled_font) pf_cairo_scaled_font_destroy;
pf_cairo_scaled_font_destroy cairo_scaled_font_destroy;
typedef void function(cairo_scaled_font_t* scaled_font, cairo_font_extents_t* extents) pf_cairo_scaled_font_extents;
pf_cairo_scaled_font_extents cairo_scaled_font_extents;
typedef void function(cairo_scaled_font_t* scaled_font, cairo_matrix_t* ctm) pf_cairo_scaled_font_get_ctm;
pf_cairo_scaled_font_get_ctm cairo_scaled_font_get_ctm;
typedef cairo_font_face_t* function(cairo_scaled_font_t* scaled_font) pf_cairo_scaled_font_get_font_face;
pf_cairo_scaled_font_get_font_face cairo_scaled_font_get_font_face;
typedef void function(cairo_scaled_font_t* scaled_font, cairo_matrix_t* font_matrix) pf_cairo_scaled_font_get_font_matrix;
pf_cairo_scaled_font_get_font_matrix cairo_scaled_font_get_font_matrix;
typedef void function(cairo_scaled_font_t* scaled_font, cairo_font_options_t* options) pf_cairo_scaled_font_get_font_options;
pf_cairo_scaled_font_get_font_options cairo_scaled_font_get_font_options;
typedef uint function(cairo_scaled_font_t* scaled_font) pf_cairo_scaled_font_get_reference_count;
pf_cairo_scaled_font_get_reference_count cairo_scaled_font_get_reference_count;
typedef cairo_font_type_t function(cairo_scaled_font_t* scaled_font) pf_cairo_scaled_font_get_type;
pf_cairo_scaled_font_get_type cairo_scaled_font_get_type;
typedef void* function(cairo_scaled_font_t* scaled_font, cairo_user_data_key_t* key) pf_cairo_scaled_font_get_user_data;
pf_cairo_scaled_font_get_user_data cairo_scaled_font_get_user_data;
typedef void function(cairo_scaled_font_t* scaled_font, cairo_glyph_t* glyphs, int num_glyphs, cairo_text_extents_t* extents) pf_cairo_scaled_font_glyph_extents;
pf_cairo_scaled_font_glyph_extents cairo_scaled_font_glyph_extents;
typedef cairo_scaled_font_t* function(cairo_scaled_font_t* scaled_font) pf_cairo_scaled_font_reference;
pf_cairo_scaled_font_reference cairo_scaled_font_reference;
typedef cairo_status_t function(cairo_scaled_font_t* scaled_font, cairo_user_data_key_t* key, void* user_data, cairo_destroy_func_t destroy) pf_cairo_scaled_font_set_user_data;
pf_cairo_scaled_font_set_user_data cairo_scaled_font_set_user_data;
typedef cairo_status_t function(cairo_scaled_font_t* scaled_font) pf_cairo_scaled_font_status;
pf_cairo_scaled_font_status cairo_scaled_font_status;
typedef void function(cairo_scaled_font_t* scaled_font, char* utf8, cairo_text_extents_t* extents) pf_cairo_scaled_font_text_extents;
pf_cairo_scaled_font_text_extents cairo_scaled_font_text_extents;
typedef void function(cairo_t* cr, char* family, cairo_font_slant_t slant, cairo_font_weight_t weight) pf_cairo_select_font_face;
pf_cairo_select_font_face cairo_select_font_face;
typedef void function(cairo_t* cr, cairo_antialias_t antialias) pf_cairo_set_antialias;
pf_cairo_set_antialias cairo_set_antialias;
typedef void function(cairo_t* cr, double* dashes, int num_dashes, double offset) pf_cairo_set_dash;
pf_cairo_set_dash cairo_set_dash;
typedef void function(cairo_t* cr, cairo_fill_rule_t fill_rule) pf_cairo_set_fill_rule;
pf_cairo_set_fill_rule cairo_set_fill_rule;
typedef void function(cairo_t* cr, cairo_font_face_t* font_face) pf_cairo_set_font_face;
pf_cairo_set_font_face cairo_set_font_face;
typedef void function(cairo_t* cr, cairo_matrix_t* matrix) pf_cairo_set_font_matrix;
pf_cairo_set_font_matrix cairo_set_font_matrix;
typedef void function(cairo_t* cr, cairo_font_options_t* options) pf_cairo_set_font_options;
pf_cairo_set_font_options cairo_set_font_options;
typedef void function(cairo_t* cr, double size) pf_cairo_set_font_size;
pf_cairo_set_font_size cairo_set_font_size;
typedef void function(cairo_t* cr, cairo_line_cap_t line_width) pf_cairo_set_line_cap;
pf_cairo_set_line_cap cairo_set_line_cap;
typedef void function(cairo_t* cr, cairo_line_join_t line_join) pf_cairo_set_line_join;
pf_cairo_set_line_join cairo_set_line_join;
typedef void function(cairo_t* cr, double width) pf_cairo_set_line_width;
pf_cairo_set_line_width cairo_set_line_width;
typedef void function(cairo_t* cr, cairo_matrix_t* matrix) pf_cairo_set_matrix;
pf_cairo_set_matrix cairo_set_matrix;
typedef void function(cairo_t* cr, double limit) pf_cairo_set_miter_limit;
pf_cairo_set_miter_limit cairo_set_miter_limit;
typedef void function(cairo_t* cr, cairo_operator_t op) pf_cairo_set_operator;
pf_cairo_set_operator cairo_set_operator;
typedef void function(cairo_t* cr, cairo_scaled_font_t* scaled_font) pf_cairo_set_scaled_font;
pf_cairo_set_scaled_font cairo_set_scaled_font;
typedef void function(cairo_t* cr, cairo_pattern_t* source) pf_cairo_set_source;
pf_cairo_set_source cairo_set_source;
typedef void function(cairo_t* cr, double red, double green, double blue) pf_cairo_set_source_rgb;
pf_cairo_set_source_rgb cairo_set_source_rgb;
typedef void function(cairo_t* cr, double red, double green, double blue, double alpha) pf_cairo_set_source_rgba;
pf_cairo_set_source_rgba cairo_set_source_rgba;
typedef void function(cairo_t* cr, cairo_surface_t* surface, double x, double y) pf_cairo_set_source_surface;
pf_cairo_set_source_surface cairo_set_source_surface;
typedef void function(cairo_t* cr, double tolerance) pf_cairo_set_tolerance;
pf_cairo_set_tolerance cairo_set_tolerance;
typedef cairo_status_t function(cairo_t* cr, cairo_user_data_key_t* key, void* user_data, cairo_destroy_func_t destroy) pf_cairo_set_user_data;
pf_cairo_set_user_data cairo_set_user_data;
typedef void function(cairo_t* cr, cairo_glyph_t* glyphs, int num_glyphs) pf_cairo_show_glyphs;
pf_cairo_show_glyphs cairo_show_glyphs;
typedef void function(cairo_t* cr) pf_cairo_show_page;
pf_cairo_show_page cairo_show_page;
typedef void function(cairo_t* cr, char* utf8) pf_cairo_show_text;
pf_cairo_show_text cairo_show_text;
typedef cairo_status_t function(cairo_t* cr) pf_cairo_status;
pf_cairo_status cairo_status;
typedef char* function(cairo_status_t status) pf_cairo_status_to_string;
pf_cairo_status_to_string cairo_status_to_string;
typedef void function(cairo_t* cr) pf_cairo_stroke;
pf_cairo_stroke cairo_stroke;
typedef cairo_bool_t function(cairo_t* cr, double* x1, double* y1, double* x2, double* y2) pf_cairo_stroke_extents;
pf_cairo_stroke_extents cairo_stroke_extents;
typedef void function(cairo_t* cr) pf_cairo_stroke_preserve;
pf_cairo_stroke_preserve cairo_stroke_preserve;
typedef cairo_surface_t* function(cairo_surface_t* other, cairo_content_t content, int width, int height) pf_cairo_surface_create_similar;
pf_cairo_surface_create_similar cairo_surface_create_similar;
typedef void function(cairo_surface_t* surface) pf_cairo_surface_destroy;
pf_cairo_surface_destroy cairo_surface_destroy;
typedef void function(cairo_surface_t* surface) pf_cairo_surface_finish;
pf_cairo_surface_finish cairo_surface_finish;
typedef void function(cairo_surface_t* surface) pf_cairo_surface_flush;
pf_cairo_surface_flush cairo_surface_flush;
typedef cairo_content_t function(cairo_surface_t* surface) pf_cairo_surface_get_content;
pf_cairo_surface_get_content cairo_surface_get_content;
typedef void function(cairo_surface_t* surface, double* x_offset, double* y_offset) pf_cairo_surface_get_device_offset;
pf_cairo_surface_get_device_offset cairo_surface_get_device_offset;
typedef void function(cairo_surface_t* surface, cairo_font_options_t *options) pf_cairo_surface_get_font_options;
pf_cairo_surface_get_font_options cairo_surface_get_font_options;
typedef uint function(cairo_surface_t* surface) pf_cairo_surface_get_reference_count;
pf_cairo_surface_get_reference_count cairo_surface_get_reference_count;
typedef cairo_surface_t function(cairo_surface_t* surface) pf_cairo_surface_get_type;
pf_cairo_surface_get_type cairo_surface_get_type;
typedef void* function(cairo_surface_t* surface, cairo_user_data_key_t* key) pf_cairo_surface_get_user_data;
pf_cairo_surface_get_user_data cairo_surface_get_user_data;
typedef void function(cairo_surface_t* surface) pf_cairo_surface_mark_dirty;
pf_cairo_surface_mark_dirty cairo_surface_mark_dirty;
typedef void function(cairo_surface_t* surface, int x, int y, int width, int height) pf_cairo_surface_mark_dirty_rectangle;
pf_cairo_surface_mark_dirty_rectangle cairo_surface_mark_dirty_rectangle;
typedef cairo_surface_t* function(cairo_surface_t* surface) pf_cairo_surface_reference;
pf_cairo_surface_reference cairo_surface_reference;
typedef void function(cairo_surface_t* surface, double x_offset, double y_offset) pf_cairo_surface_set_device_offset;
pf_cairo_surface_set_device_offset cairo_surface_set_device_offset;
typedef void function(cairo_surface_t* surface, double x_pixels_per_inch, double y_pixels_per_inch) pf_cairo_surface_set_fallback_resolution;
pf_cairo_surface_set_fallback_resolution cairo_surface_set_fallback_resolution;
typedef cairo_status_t function(cairo_surface_t* surface, cairo_user_data_key_t* ket, void* user_data, cairo_destroy_func_t destroy) pf_cairo_surface_set_user_data;
pf_cairo_surface_set_user_data cairo_surface_set_user_data;
typedef cairo_status_t function(cairo_surface_t* surface) pf_cairo_surface_status;
pf_cairo_surface_status cairo_surface_status;
typedef void function(cairo_t* cr, char* utf8, cairo_text_extents_t* extents) pf_cairo_text_extents;
pf_cairo_text_extents cairo_text_extents;
typedef void function(cairo_t* cr, char* utf8) pf_cairo_text_path;
pf_cairo_text_path cairo_text_path;
typedef void function(cairo_t* cr, cairo_matrix_t* matrix) pf_cairo_transform;
pf_cairo_transform cairo_transform;
typedef void function(cairo_t* cr, double tx, double ty) pf_cairo_translate;
pf_cairo_translate cairo_translate;
typedef void function(cairo_t* cr, double* x, double* y) pf_cairo_user_to_device;
pf_cairo_user_to_device cairo_user_to_device;
typedef void function(cairo_t* cr, double* dx, double *dy) pf_cairo_user_to_device_distance;
pf_cairo_user_to_device_distance cairo_user_to_device_distance;
typedef int function() pf_cairo_version;
pf_cairo_version cairo_version;
typedef char* function() pf_cairo_version_string;
pf_cairo_version_string cairo_version_string;
version( cairo_1_9 )
{
}

//typedef cairo_status_t function(cairo_surface_t *surface, const char *filename) pf_cairo_surface_write_to_png;
//pf_cairo_surface_write_to_png cairo_surface_write_to_png;

package void cairo_loadprocs(SharedLib lib)
{
    // cairo functions
    //Name = cast(pfName)getProc(lib, "Name");
    cairo_append_path = cast(pf_cairo_append_path)getProc(lib, "cairo_append_path");
    cairo_arc = cast(pf_cairo_arc)getProc(lib, "cairo_arc");
    cairo_arc_negative = cast(pf_cairo_arc_negative)getProc(lib, "cairo_arc_negative");
    cairo_clip = cast(pf_cairo_clip)getProc(lib, "cairo_clip");
    cairo_clip_extents = cast(pf_cairo_clip_extents)getProc(lib, "cairo_clip_extents");
    cairo_clip_preserve = cast(pf_cairo_clip_preserve)getProc(lib, "cairo_clip_preserve");
    cairo_close_path = cast(pf_cairo_close_path)getProc(lib, "cairo_close_path");
    cairo_copy_clip_rectangle_list = cast(pf_cairo_copy_clip_rectangle_list)getProc(lib, "cairo_copy_clip_rectangle_list");
    cairo_copy_page = cast(pf_cairo_copy_page)getProc(lib, "cairo_copy_page");
    cairo_copy_path = cast(pf_cairo_copy_path)getProc(lib, "cairo_copy_path");
    cairo_copy_path_flat = cast(pf_cairo_copy_path_flat)getProc(lib, "cairo_copy_path_flat");
    cairo_create = cast(pf_cairo_create)getProc(lib, "cairo_create");
    cairo_curve_to = cast(pf_cairo_curve_to)getProc(lib, "cairo_curve_to");
    cairo_destroy = cast(pf_cairo_destroy)getProc(lib, "cairo_destroy");
    cairo_device_to_user = cast(pf_cairo_device_to_user)getProc(lib, "cairo_device_to_user");
    cairo_device_to_user_distance = cast(pf_cairo_device_to_user_distance)getProc(lib, "cairo_device_to_user_distance");
    cairo_fill = cast(pf_cairo_fill)getProc(lib, "cairo_fill");
    cairo_fill_extents = cast(pf_cairo_fill_extents)getProc(lib, "cairo_fill_extents");
    cairo_fill_preserve = cast(pf_cairo_fill_preserve)getProc(lib, "cairo_fill_preserve");
    cairo_font_extents = cast(pf_cairo_font_extents)getProc(lib, "cairo_font_extents");
    cairo_font_face_destroy = cast(pf_cairo_font_face_destroy)getProc(lib, "cairo_font_face_destroy");
    cairo_font_face_get_reference_count = cast(pf_cairo_font_face_get_reference_count)getProc(lib, "cairo_font_face_get_reference_count");
    cairo_font_face_get_type = cast(pf_cairo_font_face_get_type)getProc(lib, "cairo_font_face_get_type");
    cairo_font_face_get_user_data = cast(pf_cairo_font_face_get_user_data)getProc(lib, "cairo_font_face_get_user_data");
    cairo_font_face_reference = cast(pf_cairo_font_face_reference)getProc(lib, "cairo_font_face_reference");
    cairo_font_face_set_user_data = cast(pf_cairo_font_face_set_user_data)getProc(lib, "cairo_font_face_set_user_data");
    cairo_font_face_status = cast(pf_cairo_font_face_status)getProc(lib, "cairo_font_face_status");
    cairo_font_options_copy = cast(pf_cairo_font_options_copy)getProc(lib, "cairo_font_options_copy");
    cairo_font_options_create = cast(pf_cairo_font_options_create)getProc(lib, "cairo_font_options_create");
    cairo_font_options_destroy = cast(pf_cairo_font_options_destroy)getProc(lib, "cairo_font_options_destroy");
    cairo_font_options_equal = cast(pf_cairo_font_options_equal)getProc(lib, "cairo_font_options_equal");
    cairo_font_options_get_antialias = cast(pf_cairo_font_options_get_antialias)getProc(lib, "cairo_font_options_get_antialias");
    cairo_font_options_get_hint_metrics = cast(pf_cairo_font_options_get_hint_metrics)getProc(lib, "cairo_font_options_get_hint_metrics");
    cairo_font_options_get_hint_style = cast(pf_cairo_font_options_get_hint_style)getProc(lib, "cairo_font_options_get_hint_style");
    cairo_font_options_get_subpixel_order = cast(pf_cairo_font_options_get_subpixel_order)getProc(lib, "cairo_font_options_get_subpixel_order");
    cairo_font_options_hash = cast(pf_cairo_font_options_hash)getProc(lib, "cairo_font_options_hash");
    cairo_font_options_merge = cast(pf_cairo_font_options_merge)getProc(lib, "cairo_font_options_merge");
    cairo_font_options_set_antialias = cast(pf_cairo_font_options_set_antialias)getProc(lib, "cairo_font_options_set_antialias");
    cairo_font_options_set_hint_metrics = cast(pf_cairo_font_options_set_hint_metrics)getProc(lib, "cairo_font_options_set_hint_metrics");
    cairo_font_options_set_hint_style = cast(pf_cairo_font_options_set_hint_style)getProc(lib, "cairo_font_options_set_hint_style");
    cairo_font_options_set_subpixel_order = cast(pf_cairo_font_options_set_subpixel_order)getProc(lib, "cairo_font_options_set_subpixel_order");
    cairo_font_options_status = cast(pf_cairo_font_options_status)getProc(lib, "cairo_font_options_status");
    cairo_get_antialias = cast(pf_cairo_get_antialias)getProc(lib, "cairo_get_antialias");
    cairo_get_current_point = cast(pf_cairo_get_current_point)getProc(lib, "cairo_get_current_point");
    cairo_get_dash = cast(pf_cairo_get_dash)getProc(lib, "cairo_get_dash");
    cairo_get_dash_count = cast(pf_cairo_get_dash_count)getProc(lib, "cairo_get_dash_count");
    cairo_get_fill_rule = cast(pf_cairo_get_fill_rule)getProc(lib, "cairo_get_fill_rule");
    cairo_get_font_face = cast(pf_cairo_get_font_face)getProc(lib, "cairo_get_font_face");
    cairo_get_font_matrix = cast(pf_cairo_get_font_matrix)getProc(lib, "cairo_get_font_matrix");
    cairo_get_font_options = cast(pf_cairo_get_font_options)getProc(lib, "cairo_get_font_options");
    cairo_get_group_target = cast(pf_cairo_get_group_target)getProc(lib, "cairo_get_group_target");
    cairo_get_line_cap = cast(pf_cairo_get_line_cap)getProc(lib, "cairo_get_line_cap");
    cairo_get_line_join = cast(pf_cairo_get_line_join)getProc(lib, "cairo_get_line_join");
    cairo_get_line_width = cast(pf_cairo_get_line_width)getProc(lib, "cairo_get_line_width");
    cairo_get_miter_limit = cast(pf_cairo_get_miter_limit)getProc(lib, "cairo_get_miter_limit");
    cairo_get_operator = cast(pf_cairo_get_operator)getProc(lib, "cairo_get_operator");
    cairo_get_reference_count = cast(pf_cairo_get_reference_count)getProc(lib, "cairo_get_reference_count");
    cairo_get_matrix = cast(pf_cairo_get_matrix)getProc(lib, "cairo_get_matrix");
    cairo_get_scaled_font = cast(pf_cairo_get_scaled_font)getProc(lib, "cairo_get_scaled_font");
    cairo_get_source = cast(pf_cairo_get_source)getProc(lib, "cairo_get_source");
    cairo_get_target = cast(pf_cairo_get_target)getProc(lib, "cairo_get_target");
    cairo_get_tolerance = cast(pf_cairo_get_tolerance)getProc(lib, "cairo_get_tolerance");
    cairo_get_user_data = cast(pf_cairo_get_user_data)getProc(lib, "cairo_get_user_data");
    cairo_glyph_extents = cast(pf_cairo_glyph_extents)getProc(lib, "cairo_glyph_extents");
    cairo_glyph_path = cast(pf_cairo_glyph_path)getProc(lib, "cairo_glyph_path");
    cairo_identity_matrix = cast(pf_cairo_identity_matrix)getProc(lib, "cairo_identity_matrix");
    cairo_image_surface_create = cast(pf_cairo_image_surface_create)getProc(lib, "cairo_image_surface_create");
    cairo_image_surface_create_for_data = cast(pf_cairo_image_surface_create_for_data)getProc(lib, "cairo_image_surface_create_for_data");
    cairo_image_surface_get_data = cast(pf_cairo_image_surface_get_data)getProc(lib, "cairo_image_surface_get_data");
    cairo_image_surface_get_format = cast(pf_cairo_image_surface_get_format)getProc(lib, "cairo_image_surface_get_format");
    cairo_image_surface_get_height = cast(pf_cairo_image_surface_get_height)getProc(lib, "cairo_image_surface_get_height");
    cairo_image_surface_get_stride = cast(pf_cairo_image_surface_get_stride)getProc(lib, "cairo_image_surface_get_stride");
    cairo_image_surface_get_width = cast(pf_cairo_image_surface_get_width)getProc(lib, "cairo_image_surface_get_width");
    cairo_in_fill = cast(pf_cairo_in_fill)getProc(lib, "cairo_in_fill");
    cairo_in_stroke = cast(pf_cairo_in_stroke)getProc(lib, "cairo_in_stroke");
    cairo_line_to = cast(pf_cairo_line_to)getProc(lib, "cairo_line_to");
    cairo_mask = cast(pf_cairo_mask)getProc(lib, "cairo_mask");
    cairo_mask_surface = cast(pf_cairo_mask_surface)getProc(lib, "cairo_mask_surface");
    cairo_matrix_init = cast(pf_cairo_matrix_init)getProc(lib, "cairo_matrix_init");
    cairo_matrix_init_identity = cast(pf_cairo_matrix_init_identity)getProc(lib, "cairo_matrix_init_identity");
    cairo_matrix_init_rotate = cast(pf_cairo_matrix_init_rotate)getProc(lib, "cairo_matrix_init_rotate");
    cairo_matrix_init_scale = cast(pf_cairo_matrix_init_scale)getProc(lib, "cairo_matrix_init_scale");
    cairo_matrix_init_translate = cast(pf_cairo_matrix_init_translate)getProc(lib, "cairo_matrix_init_translate");
    cairo_matrix_invert = cast(pf_cairo_matrix_invert)getProc(lib, "cairo_matrix_invert");
    cairo_matrix_multiply = cast(pf_cairo_matrix_multiply)getProc(lib, "cairo_matrix_multiply");
    cairo_matrix_rotate = cast(pf_cairo_matrix_rotate)getProc(lib, "cairo_matrix_rotate");
    cairo_matrix_scale = cast(pf_cairo_matrix_scale)getProc(lib, "cairo_matrix_scale");
    cairo_matrix_transform_distance = cast(pf_cairo_matrix_transform_distance)getProc(lib, "cairo_matrix_transform_distance");
    cairo_matrix_transform_point = cast(pf_cairo_matrix_transform_point)getProc(lib, "cairo_matrix_transform_point");
    cairo_matrix_translate = cast(pf_cairo_matrix_translate)getProc(lib, "cairo_matrix_translate");
    cairo_move_to = cast(pf_cairo_move_to)getProc(lib, "cairo_move_to");
    cairo_new_path = cast(pf_cairo_new_path)getProc(lib, "cairo_new_path");
    cairo_new_sub_path = cast(pf_cairo_new_sub_path)getProc(lib, "cairo_new_sub_path");
    cairo_paint = cast(pf_cairo_paint)getProc(lib, "cairo_paint");
    cairo_paint_with_alpha = cast(pf_cairo_paint_with_alpha)getProc(lib, "cairo_paint_with_alpha");
    cairo_path_destroy = cast(pf_cairo_path_destroy)getProc(lib, "cairo_path_destroy");
    cairo_pattern_add_color_stop_rgb = cast(pf_cairo_pattern_add_color_stop_rgb)getProc(lib, "cairo_pattern_add_color_stop_rgb");
    cairo_pattern_add_color_stop_rgba = cast(pf_cairo_pattern_add_color_stop_rgba)getProc(lib, "cairo_pattern_add_color_stop_rgba");
    cairo_pattern_create_for_surface = cast(pf_cairo_pattern_create_for_surface)getProc(lib, "cairo_pattern_create_for_surface");
    cairo_pattern_create_linear = cast(pf_cairo_pattern_create_linear)getProc(lib, "cairo_pattern_create_linear");
    cairo_pattern_create_radial = cast(pf_cairo_pattern_create_radial)getProc(lib, "cairo_pattern_create_radial");
    cairo_pattern_create_rgb = cast(pf_cairo_pattern_create_rgb)getProc(lib, "cairo_pattern_create_rgb");
    cairo_pattern_create_rgba = cast(pf_cairo_pattern_create_rgba)getProc(lib, "cairo_pattern_create_rgba");
    cairo_pattern_destroy = cast(pf_cairo_pattern_destroy)getProc(lib, "cairo_pattern_destroy");
    cairo_pattern_get_color_stop_count = cast(pf_cairo_pattern_get_color_stop_count)getProc(lib, "cairo_pattern_get_color_stop_count");
    cairo_pattern_get_color_stop_rgba = cast(pf_cairo_pattern_get_color_stop_rgba)getProc(lib, "cairo_pattern_get_color_stop_rgba");
    cairo_pattern_get_extend = cast(pf_cairo_pattern_get_extend)getProc(lib, "cairo_pattern_get_extend");
    cairo_pattern_get_filter = cast(pf_cairo_pattern_get_filter)getProc(lib, "cairo_pattern_get_filter");
    cairo_pattern_get_linear_points = cast(pf_cairo_pattern_get_linear_points)getProc(lib, "cairo_pattern_get_linear_points");
    cairo_pattern_get_matrix = cast(pf_cairo_pattern_get_matrix)getProc(lib, "cairo_pattern_get_matrix");
    cairo_pattern_get_radial_circles = cast(pf_cairo_pattern_get_radial_circles)getProc(lib, "cairo_pattern_get_radial_circles");
    cairo_pattern_get_reference_count = cast(pf_cairo_pattern_get_reference_count)getProc(lib, "cairo_pattern_get_reference_count");
    cairo_pattern_get_rgba = cast(pf_cairo_pattern_get_rgba)getProc(lib, "cairo_pattern_get_rgba");
    cairo_pattern_get_surface = cast(pf_cairo_pattern_get_surface)getProc(lib, "cairo_pattern_get_surface");
    cairo_pattern_get_type = cast(pf_cairo_pattern_get_type)getProc(lib, "cairo_pattern_get_type");
    cairo_pattern_get_user_data = cast(pf_cairo_pattern_get_user_data)getProc(lib, "cairo_pattern_get_user_data");
    cairo_pattern_reference = cast(pf_cairo_pattern_reference)getProc(lib, "cairo_pattern_reference");
    cairo_pattern_set_extend = cast(pf_cairo_pattern_set_extend)getProc(lib, "cairo_pattern_set_extend");
    cairo_pattern_set_filter = cast(pf_cairo_pattern_set_filter)getProc(lib, "cairo_pattern_set_filter");
    cairo_pattern_set_matrix = cast(pf_cairo_pattern_set_matrix)getProc(lib, "cairo_pattern_set_matrix");
    cairo_pattern_set_user_data = cast(pf_cairo_pattern_set_user_data)getProc(lib, "cairo_pattern_set_user_data");
    cairo_pattern_status = cast(pf_cairo_pattern_status)getProc(lib, "cairo_pattern_status");
    cairo_pop_group = cast(pf_cairo_pop_group)getProc(lib, "cairo_pop_group");
    cairo_pop_group_to_source = cast(pf_cairo_pop_group_to_source)getProc(lib, "cairo_pop_group_to_source");
    cairo_push_group = cast(pf_cairo_push_group)getProc(lib, "cairo_push_group");
    cairo_push_group_with_content = cast(pf_cairo_push_group_with_content)getProc(lib, "cairo_push_group_with_content");
    cairo_rectangle = cast(pf_cairo_rectangle)getProc(lib, "cairo_rectangle");
    cairo_rectangle_list_destroy = cast(pf_cairo_rectangle_list_destroy)getProc(lib, "cairo_rectangle_list_destroy");
    cairo_reference = cast(pf_cairo_reference)getProc(lib, "cairo_reference");
    cairo_rel_curve_to = cast(pf_cairo_rel_curve_to)getProc(lib, "cairo_rel_curve_to");
    cairo_rel_line_to = cast(pf_cairo_rel_line_to)getProc(lib, "cairo_rel_line_to");
    cairo_rel_move_to = cast(pf_cairo_rel_move_to)getProc(lib, "cairo_rel_move_to");
    cairo_reset_clip = cast(pf_cairo_reset_clip)getProc(lib, "cairo_reset_clip");
    cairo_restore = cast(pf_cairo_restore)getProc(lib, "cairo_restore");
    cairo_rotate = cast(pf_cairo_rotate)getProc(lib, "cairo_rotate");
    cairo_save = cast(pf_cairo_save)getProc(lib, "cairo_save");
    cairo_scale = cast(pf_cairo_scale)getProc(lib, "cairo_scale");
    cairo_scaled_font_create = cast(pf_cairo_scaled_font_create)getProc(lib, "cairo_scaled_font_create");
    cairo_scaled_font_destroy = cast(pf_cairo_scaled_font_destroy)getProc(lib, "cairo_scaled_font_destroy");
    cairo_scaled_font_extents = cast(pf_cairo_scaled_font_extents)getProc(lib, "cairo_scaled_font_extents");
    cairo_scaled_font_get_ctm = cast(pf_cairo_scaled_font_get_ctm)getProc(lib, "cairo_scaled_font_get_ctm");
    cairo_scaled_font_get_font_face = cast(pf_cairo_scaled_font_get_font_face)getProc(lib, "cairo_scaled_font_get_font_face");
    cairo_scaled_font_get_font_matrix = cast(pf_cairo_scaled_font_get_font_matrix)getProc(lib, "cairo_scaled_font_get_font_matrix");
    cairo_scaled_font_get_font_options = cast(pf_cairo_scaled_font_get_font_options)getProc(lib, "cairo_scaled_font_get_font_options");
    cairo_scaled_font_get_reference_count = cast(pf_cairo_scaled_font_get_reference_count)getProc(lib, "cairo_scaled_font_get_reference_count");
    cairo_scaled_font_get_type = cast(pf_cairo_scaled_font_get_type)getProc(lib, "cairo_scaled_font_get_type");
    cairo_scaled_font_get_user_data = cast(pf_cairo_scaled_font_get_user_data)getProc(lib, "cairo_scaled_font_get_user_data");
    cairo_scaled_font_glyph_extents = cast(pf_cairo_scaled_font_glyph_extents)getProc(lib, "cairo_scaled_font_glyph_extents");
    cairo_scaled_font_reference = cast(pf_cairo_scaled_font_reference)getProc(lib, "cairo_scaled_font_reference");
    cairo_scaled_font_set_user_data = cast(pf_cairo_scaled_font_set_user_data)getProc(lib, "cairo_scaled_font_set_user_data");
    cairo_scaled_font_status = cast(pf_cairo_scaled_font_status)getProc(lib, "cairo_scaled_font_status");
    cairo_scaled_font_text_extents = cast(pf_cairo_scaled_font_text_extents)getProc(lib, "cairo_scaled_font_text_extents");
    cairo_select_font_face = cast(pf_cairo_select_font_face)getProc(lib, "cairo_select_font_face");
    cairo_set_antialias = cast(pf_cairo_set_antialias)getProc(lib, "cairo_set_antialias");
    cairo_set_dash = cast(pf_cairo_set_dash)getProc(lib, "cairo_set_dash");
    cairo_set_fill_rule = cast(pf_cairo_set_fill_rule)getProc(lib, "cairo_set_fill_rule");
    cairo_set_font_face = cast(pf_cairo_set_font_face)getProc(lib, "cairo_set_font_face");
    cairo_set_font_matrix = cast(pf_cairo_set_font_matrix)getProc(lib, "cairo_set_font_matrix");
    cairo_set_font_options = cast(pf_cairo_set_font_options)getProc(lib, "cairo_set_font_options");
    cairo_set_font_size = cast(pf_cairo_set_font_size)getProc(lib, "cairo_set_font_size");
    cairo_set_line_cap = cast(pf_cairo_set_line_cap)getProc(lib, "cairo_set_line_cap");
    cairo_set_line_join = cast(pf_cairo_set_line_join)getProc(lib, "cairo_set_line_join");
    cairo_set_line_width = cast(pf_cairo_set_line_width)getProc(lib, "cairo_set_line_width");
    cairo_set_matrix = cast(pf_cairo_set_matrix)getProc(lib, "cairo_set_matrix");
    cairo_set_miter_limit = cast(pf_cairo_set_miter_limit)getProc(lib, "cairo_set_miter_limit");
    cairo_set_operator = cast(pf_cairo_set_operator)getProc(lib, "cairo_set_operator");
    cairo_set_scaled_font = cast(pf_cairo_set_scaled_font)getProc(lib, "cairo_set_scaled_font");
    cairo_set_source = cast(pf_cairo_set_source)getProc(lib, "cairo_set_source");
    cairo_set_source_rgb = cast(pf_cairo_set_source_rgb)getProc(lib, "cairo_set_source_rgb");
    cairo_set_source_rgba = cast(pf_cairo_set_source_rgba)getProc(lib, "cairo_set_source_rgba");
    cairo_set_source_surface = cast(pf_cairo_set_source_surface)getProc(lib, "cairo_set_source_surface");
    cairo_set_tolerance = cast(pf_cairo_set_tolerance)getProc(lib, "cairo_set_tolerance");
    cairo_set_user_data = cast(pf_cairo_set_user_data)getProc(lib, "cairo_set_user_data");
    cairo_show_glyphs = cast(pf_cairo_show_glyphs)getProc(lib, "cairo_show_glyphs");
    cairo_show_page = cast(pf_cairo_show_page)getProc(lib, "cairo_show_page");
    cairo_show_text = cast(pf_cairo_show_text)getProc(lib, "cairo_show_text");
    cairo_status = cast(pf_cairo_status)getProc(lib, "cairo_status");
    cairo_status_to_string = cast(pf_cairo_status_to_string)getProc(lib, "cairo_status_to_string");
    cairo_stroke = cast(pf_cairo_stroke)getProc(lib, "cairo_stroke");
    cairo_stroke_extents = cast(pf_cairo_stroke_extents)getProc(lib, "cairo_stroke_extents");
    cairo_stroke_preserve = cast(pf_cairo_stroke_preserve)getProc(lib, "cairo_stroke_preserve");
    cairo_surface_create_similar = cast(pf_cairo_surface_create_similar)getProc(lib, "cairo_surface_create_similar");
    cairo_surface_destroy = cast(pf_cairo_surface_destroy)getProc(lib, "cairo_surface_destroy");
    cairo_surface_finish = cast(pf_cairo_surface_finish)getProc(lib, "cairo_surface_finish");
    cairo_surface_flush = cast(pf_cairo_surface_flush)getProc(lib, "cairo_surface_flush");
    cairo_surface_get_content = cast(pf_cairo_surface_get_content)getProc(lib, "cairo_surface_get_content");
    cairo_surface_get_device_offset = cast(pf_cairo_surface_get_device_offset)getProc(lib, "cairo_surface_get_device_offset");
    cairo_surface_get_font_options = cast(pf_cairo_surface_get_font_options)getProc(lib, "cairo_surface_get_font_options");
    cairo_surface_get_reference_count = cast(pf_cairo_surface_get_reference_count)getProc(lib, "cairo_surface_get_reference_count");
    cairo_surface_get_type = cast(pf_cairo_surface_get_type)getProc(lib, "cairo_surface_get_type");
    cairo_surface_get_user_data = cast(pf_cairo_surface_get_user_data)getProc(lib, "cairo_surface_get_user_data");
    cairo_surface_mark_dirty = cast(pf_cairo_surface_mark_dirty)getProc(lib, "cairo_surface_mark_dirty");
    cairo_surface_mark_dirty_rectangle = cast(pf_cairo_surface_mark_dirty_rectangle)getProc(lib, "cairo_surface_mark_dirty_rectangle");
    cairo_surface_reference = cast(pf_cairo_surface_reference)getProc(lib, "cairo_surface_reference");
    cairo_surface_set_device_offset = cast(pf_cairo_surface_set_device_offset)getProc(lib, "cairo_surface_set_device_offset");
    cairo_surface_set_fallback_resolution = cast(pf_cairo_surface_set_fallback_resolution)getProc(lib, "cairo_surface_set_fallback_resolution");
    cairo_surface_set_user_data = cast(pf_cairo_surface_set_user_data)getProc(lib, "cairo_surface_set_user_data");
    cairo_surface_status = cast(pf_cairo_surface_status)getProc(lib, "cairo_surface_status");
    cairo_text_extents = cast(pf_cairo_text_extents)getProc(lib, "cairo_text_extents");
    cairo_text_path = cast(pf_cairo_text_path)getProc(lib, "cairo_text_path");
    cairo_transform = cast(pf_cairo_transform)getProc(lib, "cairo_transform");
    cairo_translate = cast(pf_cairo_translate)getProc(lib, "cairo_translate");
    cairo_user_to_device = cast(pf_cairo_user_to_device)getProc(lib, "cairo_user_to_device");
    cairo_user_to_device_distance = cast(pf_cairo_user_to_device_distance)getProc(lib, "cairo_user_to_device_distance");
    cairo_version = cast(pf_cairo_version)getProc(lib, "cairo_version");
    cairo_version_string = cast(pf_cairo_version_string)getProc(lib, "cairo_version_string");
    version( cairo_1_9 )
    {
    }


    //cairo_surface_write_to_png = cast(pf_cairo_surface_write_to_png)getProc(lib, "cairo_surface_write_to_png");
}

