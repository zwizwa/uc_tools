#ifndef MOD_TUI_SERVER
#define MOD_TUI_SERVER



/* Serve the TAG_U32 TUI protocol. */
/* Handle draw commands. */
struct tui_window *assert_window(uint32_t wid) {
    ASSERT(wid < TUI_MAX_NB_WINDOWS);
    ASSERT(window[wid]);
    return window[wid];
}


int draw(struct tag_u32 *req) {
    /* Optional */
    TAG_U32_MATCH(req, TUI_CMD_INIT_SCREEN, m, cols, lines) {
        LOG("init_screen: %d %d\n", m->cols, m->lines);
        tui_init_screen(m->cols, m->lines);
        return 0;
    }

    /* Client side is doing window ID allocation and we just follow.
       The TUI_MAX_NB_WINDOWS is shared via tui.h */
    TAG_U32_MATCH(req, TUI_CMD_NEW_WINDOW, m, wid, w, h, x, y) {
        // LOG("new_window: %d %d %d %d %d\n", m->w, m->h, m->x, m->y, m->wid);
        ASSERT(m->wid < TUI_MAX_NB_WINDOWS);
        ASSERT(!window[m->wid]);
        window[m->wid] = tui_new_window(m->w,m->h,m->x,m->y);
        struct tui_window *w = assert_window(m->wid);
        w->id = m->wid;
        return 0;
    }
    TAG_U32_MATCH(req, TUI_CMD_DEL_WINDOW, m, wid) {
        // LOG("del_window: %d\n", m->wid);
        struct tui_window *w = assert_window(m->wid);
        tui_del_window(w);
        window[m->wid] = NULL;
        return 0;
    }

    /* GENERIC */
    TAG_U32_MATCH(req, TUI_CMD_STRING_AT, m, wid, x, y, width) {
        /* Check values, pad string, pass it to local tui C api. */
        struct tui_window *w = assert_window(m->wid);
        char str[req->nb_bytes+1];
        memcpy(str, req->bytes, req->nb_bytes);
        str[req->nb_bytes] = 0;
        tui_string_at(window[m->wid], m->x, m->y, m->width, str);
        return 0;
    }
    TAG_U32_MATCH(req, TUI_CMD_REVERSE_VIDEO, m, wid, mode) {
        // LOG("reverse_video: %d %d\n", m->wid, m->mode);
        struct tui_window *w = assert_window(m->wid);
        tui_reverse_video(w, m->mode);
        return 0;
    }
    TAG_U32_MATCH(req, TUI_CMD_CLEAR, m, wid) {
        // LOG("clear: %d\n", m->wid);
        struct tui_window *w = assert_window(m->wid);
        tui_clear(w);
        return 0;
    }
    TAG_U32_MATCH(req, TUI_CMD_BOX, m, wid) {
        // LOG("box: %d\n", m->wid);
        struct tui_window *w = assert_window(m->wid);
        tui_box(w);
        return 0;
    }
    TAG_U32_MATCH(req, TUI_CMD_SCROLL_WINDOW, m, wid, lines) {
        // LOG("scroll_window: %d %d\n", m->wid, m->lines);
        struct tui_window *w = assert_window(m->wid);
        tui_scroll(w, m->lines);
        return 0;
    }
    TAG_U32_MATCH_0(req, TUI_CMD_UPDATE_SCREEN) {
        tui_update_screen();
        return 0;
    }
    log_tag_u32("tui_server: draw:", req);
    return 0;
}
#define DEF_MAP DEF_TAG_U32_CONST_MAP_HANDLE

DEF_MAP(
    map_root,
    /* 0 */ {"draw", "cmd", draw},
    )

#endif
