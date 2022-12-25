# Script used to generate language stats on the repo
# depends on the github-linguist ruby gem, which is assumed to be in path or set in the LINGUIST environment variable
# if not found, the script simply exits

import os
import subprocess
import json
import yaml
import requests
import cairo
import math

LANGUAGE_YML_URL = "https://raw.githubusercontent.com/github/linguist/master/lib/linguist/languages.yml"
COLORS = {
    'dark': {
        #'background': [0.086, 0.105, 0.133],
        'background': [0.000, 0.000, 0.000, 0.000],
        'gutter': [0.203, 0.223, 0.254],
        'text': [0.788, 0.819, 0.850],
        'percentage': [0.545, 0.580, 0.619],
    },
    'light': {
        #'background': [1.000, 1.000, 1.000],
        'background': [0.000, 0.000, 0.000, 0.000],
        'gutter': [0.937, 0.945, 0.952],
        'text': [0.141, 0.160, 0.184],
        'percentage': [0.341, 0.376, 0.415],
    }
}

def fetch_langs() -> str:
    res = requests.get(LANGUAGE_YML_URL)
    return res.text


def get_langs() -> str:
    try:
        read = open(".langs.yml")
        langs = read.read()
        read.close()
        return langs
    except:
        langs = fetch_langs()
        write = open(".langs.yml", "w")
        write.write(langs)
        write.close()
        return langs

def run_linguist(ghlin: str) -> str:
    try:
        proc = subprocess.run([ghlin, "--json"], stdout=subprocess.PIPE)
        return proc.stdout.decode("utf-8")
    except:
        print(f"Error when trying to run github-linguist (command: {ghlin}), aborting langs.py");
        exit(0)

def set_color(ctx, color: list[float]):
    if len(color) == 4:
        ctx.set_source_rgba(*color)
    elif len(color) == 3:
        ctx.set_source_rgb(*color)
    elif len(color) == 1:
        ctx.set_source_rgb(color[0], color[0], color[0])
    else:
        raise ValueError("color array isn't of length 4, 3, 2 or 1")

def hex_to_rgb(code: str) -> list[float]:
    hex = code.lstrip("#")
    return [int(hex[i:i+2], base=16) / 255 for i in range(0, len(hex), 2)]

def render(path: str, colors: dict, data: dict):
    # svg generation

    WIDTH, HEIGHT = 410, 400

    HALF_PI = 0.5 * math.pi
    TWO_PI = 2 * math.pi

    GUTTER_WIDTH = 0
    BAR_RADIUS = 5
    PADDING = (5, 20)
    FONT_SIZE = 14
    LINE_HEIGHT = 24
    DOT_RADIUS = 4
    DOT_OFFSET = 4
    DOT_MARGIN = 8
    FONT = 'Arial'
    SPACING = 30

    y_len = PADDING[1] * 2 + 2 * BAR_RADIUS
    x_len = PADDING[0] * 3 + DOT_RADIUS * 2 + DOT_MARGIN

    # compute canvas size
    surf_sample = cairo.ImageSurface(cairo.FORMAT_ARGB32, WIDTH, HEIGHT)
    ctx_sample = cairo.Context(surf_sample)

    ctx_sample.select_font_face(FONT, cairo.FONT_SLANT_NORMAL, cairo.FONT_WEIGHT_NORMAL)
    ctx_sample.set_font_size(FONT_SIZE)

    for language in data:
        lang_length = ctx_sample.text_extents(language + (" %.2f%%" % (data[language]['fac'] * 100)))[2] + SPACING + 2 * DOT_RADIUS + DOT_MARGIN

        if x_len + lang_length >= WIDTH:
            x_len = PADDING[0] * 4 + 2 * DOT_RADIUS
            y_len += LINE_HEIGHT

        x_len += lang_length

    # create context with correct size
    surface = cairo.SVGSurface(path, WIDTH, y_len - LINE_HEIGHT) # create some bottom space
    ctx = cairo.Context(surface)

    # background
    ctx.rectangle(0, 0, 410, 400)
    set_color(ctx, colors['background'])
    ctx.fill()

    # bar

    # clip
    x = PADDING[0]

    ctx.arc(x + BAR_RADIUS, PADDING[1] + BAR_RADIUS, BAR_RADIUS, HALF_PI, -HALF_PI)
    ctx.arc(WIDTH - PADDING[0] - BAR_RADIUS, PADDING[1] + BAR_RADIUS, BAR_RADIUS, -HALF_PI, HALF_PI)
    ctx.clip()

    last_language = next(reversed(data.keys()))
    
    # colored rectangles
    for language in data:
        width = data[language]['fac'] * (WIDTH - 2 * PADDING[0])

        set_color(ctx, data[language]['color'])
        ctx.rectangle(x, PADDING[1], width, BAR_RADIUS * 2)
        ctx.fill()

        x += width

        if language != last_language and GUTTER_WIDTH > 0:
            set_color(ctx, colors['gutter'])
            ctx.rectangle(x - GUTTER_WIDTH,PADDING[1], GUTTER_WIDTH, BAR_RADIUS * 2)
            ctx.fill()

    ctx.reset_clip()

    # text and colored dots

    text_x = PADDING[0] * 3 + DOT_RADIUS * 2 + DOT_MARGIN
    text_y = PADDING[1] * 2 + BAR_RADIUS * 2
    text_height = 0

    # for text extent
    ctx.select_font_face(FONT, cairo.FONT_SLANT_NORMAL, cairo.FONT_WEIGHT_NORMAL)
    ctx.set_font_size(FONT_SIZE)
    for language in data:
        percentage = "%.02f%%" % (data[language]['fac'] * 100)
        label_width = ctx.text_extents(language + " ")[2]
        percent_width = ctx.text_extents(percentage)[2]
        text_width = label_width + percent_width + SPACING + 2 * DOT_RADIUS + DOT_MARGIN

        if text_x + text_width >= WIDTH:
            text_x = PADDING[0] * 3 + DOT_RADIUS * 2 + DOT_MARGIN
            text_y += LINE_HEIGHT

        # dot
        set_color(ctx, data[language]['color'])
        ctx.arc(text_x - DOT_RADIUS * 2 - DOT_MARGIN, text_y - DOT_OFFSET, DOT_RADIUS, 0, TWO_PI)
        ctx.fill()

        # draw the text for that language
        set_color(ctx, colors['text'])
        ctx.select_font_face(FONT, cairo.FONT_SLANT_NORMAL, cairo.FONT_WEIGHT_BOLD)
        ctx.set_font_size(FONT_SIZE)

        ctx.move_to(text_x, text_y)
        ctx.show_text(language + " ")

        set_color(ctx, colors['percentage'])
        ctx.select_font_face(FONT, cairo.FONT_SLANT_NORMAL, cairo.FONT_WEIGHT_NORMAL)
        ctx.set_font_size(FONT_SIZE)
        ctx.show_text(percentage)

        text_x += text_width

ghlin = os.getenv("LINGUIST")
if ghlin is None:
    ghlin = "github-linguist"

linguist_out = json.loads(run_linguist(ghlin))
langs = yaml.safe_load(get_langs())
data = {
    k: {'fac': float(linguist_out[k]['percentage']) / 100.0, 'color': hex_to_rgb(langs[k]['color'])}
    for k in sorted(linguist_out, reverse=True, key=lambda k: linguist_out[k]["size"])
}

render(".github/langs_dark.svg", COLORS['dark'], data)
render(".github/langs_light.svg", COLORS['light'], data)
