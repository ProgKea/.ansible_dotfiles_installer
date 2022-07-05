/* See LICENSE file for copyright and license details. */

/* appearance */
static const unsigned int borderpx = 2; /* border pixel size of windows */
static const unsigned int gappx = 0;    /* gaps size between windows */
static const unsigned int snap = 32;    /* snap pixel */
static const int showbar = 0;           /* 0 means no bar */
static const int topbar = 1;            /* 0 means bottom bar */
// static const char *fonts[]          = { "Ubuntu Mono Nerd Font:size=17" };
// static const char dmenufont[]       = "Ubuntu Mono Nerd Font:size=17";
static const char *fonts[] = {"Iosevka Nerd Font:size=11"};
static const char dmenufont[] = "Iosevka Nerd Font:size=11";
// background color
static const char col_gray1[] = "#222222";
// inactive window border color
static const char col_gray2[] = "#444444";
// font color
static const char col_gray3[] = "#bbbbbb";
// current tag and current window font color
static const char col_gray4[] = "#eeeeee";
// Top bar second color (blue) and active window border color
static const char col_cyan[] = "#005577";
static const char *colors[][3] = {
    /*               fg         bg         border   */
    [SchemeNorm] = {col_gray3, col_gray1, col_gray2},
    [SchemeSel] = {col_gray4, col_cyan, col_cyan},
};

//
/* tagging */
// tag names (upper left)
static const char *tags[] = {"1", "2", "3", "4", "5", "6", "7", "8", "9"};

static const Rule rules[] = {
    /* xprop(1):
     *	WM_CLASS(STRING) = instance, class
     *	WM_NAME(STRING) = title
     */
    /* class      instance    title       tags mask     isfloating   monitor */
    {"Gimp", NULL, NULL, 0, 1, -1},
    {"Firefox", NULL, NULL, 1 << 8, 0, -1},
};

/* layout(s) */
static const float mfact = 0.5; /* factor of master area size [0.05..0.95] */
static const int nmaster = 1;   /* number of clients in master area */
static const int resizehints =
    0; /* 1 means respect size hints in tiled resizals */

static const Layout layouts[] = {
    /* symbol     arrange function */
    {"[]=", tile}, /* first entry is default */
    {"><>", NULL}, /* no layout function means floating behavior */
    {"[M]", monocle},
};

/* key definitions */
#define MODKEY Mod1Mask
#define TAGKEYS(KEY, TAG)                                                      \
  {MODKEY, KEY, view, {.ui = 1 << TAG}},                                       \
      {MODKEY | ControlMask, KEY, toggleview, {.ui = 1 << TAG}},               \
      {MODKEY | ShiftMask, KEY, tag, {.ui = 1 << TAG}},                        \
      {MODKEY | ControlMask | ShiftMask, KEY, toggletag, {.ui = 1 << TAG}},

/* helper for spawning shell commands in the pre dwm-5.0 fashion */
#define SHCMD(cmd)                                                             \
  {                                                                            \
    .v = (const char *[]) { "/bin/sh", "-c", cmd, NULL }                       \
  }

/* commands */
static char dmenumon[2] =
    "0"; /* component of dmenucmd, manipulated in spawn() */
// static const char *dmenucmd[] = { "dmenu_run", "-m", dmenumon, "-fn",
// dmenufont, "-nb", col_gray1, "-nf", col_gray3, "-sb", col_cyan, "-sf",
// col_gray4, NULL }; //, col_gray1, "-nf", col_gray3, "-sb", col_cyan, "-sf",
// col_gray4, static const char *dmenucmd[] = { "dmenu_run", "-m", dmenumon,
// "-fn", dmenufont, "-nb", norm_bg, "-nf", norm_fg, "-sb", sel_bg, "-sf",
// norm_fg, NULL }; //, col_gray1, "-nf", col_gray3, "-sb", col_cyan, "-sf",
// col_gray4,
static const char *dmenucmd[] = {"dmenu_run", "-m",      dmenumon,
                                 "-fn",       dmenufont, NULL};
static const char *termcmd[] = {"st", NULL};
//static const char *termcmd[] = {"alacritty", NULL};
//static const char *termcmd[] = {"urxvtc", NULL};
//static const char *termcmd[] = {"kitty", NULL};
static const char *google[] = {"google", NULL};
static const char *web[] = {"visitWebsite", NULL};
static const char *yt[] = {"yt", "-g", NULL};
static const char *ytMute[] = {"yt", "-g", "-m", NULL};
static const char *killer[] = {"killer", NULL};
// volume controls
static const char *upvol[] = {"pactl", "set-sink-volume", "@DEFAULT_SINK@",
                              "+10%", NULL};
static const char *downvol[] = {"pactl", "set-sink-volume", "@DEFAULT_SINK@",
                                "-10%", NULL};
static const char *mutevol[] = {"pactl", "set-sink-mute", "@DEFAULT_SINK@",
                                "toggle", NULL};
static const char *mutemic[] = {"pactl", "set-source-mute", "@DEFAULT_SINK@",
                                "toggle", NULL};

#include "shiftview.c"
static char *endx[] = {"/bin/sh", "-c", "endx", "externalpipe", NULL};
static Key keys[] = {
    /* modifier                     key        function        argument */
    {MODKEY, XK_p, spawn, {.v = dmenucmd}},
    {MODKEY | ShiftMask, XK_Return, spawn, {.v = termcmd}},
    {MODKEY | ShiftMask, XK_w, spawn, {.v = google}},
    {MODKEY, XK_w, spawn, {.v = web}},
    {MODKEY | ShiftMask, XK_e, spawn, {.v = yt}},
    {MODKEY, XK_e, spawn, {.v = ytMute}},
    {MODKEY | ShiftMask, XK_t, togglebar, {0}},
    {MODKEY, XK_j, focusstack, {.i = +1}},
    {MODKEY, XK_k, focusstack, {.i = -1}},
    {MODKEY, XK_i, incnmaster, {.i = +1}},
    {MODKEY, XK_u, incnmaster, {.i = -1}},
    {MODKEY, XK_h, setmfact, {.f = -0.05}},
    {MODKEY, XK_l, setmfact, {.f = +0.05}},
    {MODKEY, XK_z, zoom, {0}},
    {MODKEY, XK_Tab, view, {0}},
    {MODKEY | ShiftMask, XK_c, killclient, {0}},
    {MODKEY, XK_t, setlayout, {.v = &layouts[0]}},
    {MODKEY | ShiftMask, XK_m, setlayout, {.v = &layouts[1]}},
    {MODKEY, XK_f, setlayout, {.v = &layouts[2]}},
    {MODKEY | ShiftMask, XK_space, setlayout, {0}},
    {MODKEY | ShiftMask, XK_o, togglefloating, {0}},
    {MODKEY, XK_0, view, {.ui = ~0}},
    {MODKEY | ShiftMask, XK_0, tag, {.ui = ~0}},
    {MODKEY, XK_comma, focusmon, {.i = -1}},
    {MODKEY, XK_period, focusmon, {.i = +1}},
    {MODKEY | ShiftMask, XK_comma, tagmon, {.i = -1}},
    {MODKEY | ShiftMask, XK_period, tagmon, {.i = +1}},
    {MODKEY, XK_n, shiftview, {.i = +1}},
    {MODKEY, XK_b, shiftview, {.i = -1}},
    {MODKEY, XK_F8, spawn, {.v = upvol}},
    {MODKEY, XK_F7, spawn, {.v = downvol}},
    {MODKEY, XK_F6, spawn, {.v = mutevol}},
    {MODKEY, XK_F5, spawn, {.v = mutemic}},
    {MODKEY, XK_F4, spawn, {.v = killer}},
    TAGKEYS(XK_1, 0) TAGKEYS(XK_2, 1) TAGKEYS(XK_3, 2) TAGKEYS(XK_4, 3)
        TAGKEYS(XK_5, 4) TAGKEYS(XK_6, 5) TAGKEYS(XK_7, 6) TAGKEYS(XK_8, 7)
            TAGKEYS(XK_9, 8){MODKEY | ShiftMask, XK_q, quit, {0}},
};

/* button definitions */
/* click can be ClkTagBar, ClkLtSymbol, ClkStatusText, ClkWinTitle,
 * ClkClientWin, or ClkRootWin */
static Button buttons[] = {
    /* click                event mask      button          function argument
     */
    {ClkLtSymbol, 0, Button1, setlayout, {0}},
    {ClkLtSymbol, 0, Button3, setlayout, {.v = &layouts[2]}},
    {ClkWinTitle, 0, Button2, zoom, {0}},
    {ClkStatusText, 0, Button2, spawn, {.v = termcmd}},
    {ClkClientWin, MODKEY, Button1, movemouse, {0}},
    {ClkClientWin, MODKEY, Button2, togglefloating, {0}},
    {ClkClientWin, MODKEY, Button3, resizemouse, {0}},
    {ClkTagBar, 0, Button1, view, {0}},
    {ClkTagBar, 0, Button3, toggleview, {0}},
    {ClkTagBar, MODKEY, Button1, tag, {0}},
    {ClkTagBar, MODKEY, Button3, toggletag, {0}},
};
