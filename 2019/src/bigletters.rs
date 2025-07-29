use itertools::Itertools;

const LETTERS_STR: &str = "\
     ##  ###   ##  ###  #### ####  ##  #  # ###   ## #  # #    #   # #  #  ##  ###   ##   ###   ### ### #  # #   # #   # #   # #   # #### \n\
    #  # #  # #  # #  # #    #    #  # #  #  #     # # #  #    ## ## ## # #  # #  # #  #  #  # #     #  #  # #   # #   #  # #  #   #    # \n\
    #  # ###  #    #  # ###  ###  #    ####  #     # ##   #    # # # # ## #  # #  # #  #  #  # #     #  #  # #   # #   #   #    # #    #  \n\
    #### #  # #    #  # #    #    # ## #  #  #     # # #  #    #   # #  # #  # ###  #  #  ###   ##   #  #  # #   # # # #  # #    #    #   \n\
    #  # #  # #  # #  # #    #    #  # #  #  #  #  # # #  #    #   # #  # #  # #    # ##  # #     #  #  #  #  # #  ## ## #   #   #   #    \n\
    #  # ###   ##  ###  #### #     ### #  # ###  ##  #  # #### #   # #  #  ##  #     # ## #  # ###   #   ##    #   #   # #   #   #   #### \
    ";
const LETTERS_BITMAPS: [u32; 26] = {
    let mut bits = [false; 134 * 6];
    str_to_bitmap(LETTERS_STR, '#', &mut bits);
    let mut buf = [0; 26];
    parse_bitmap(&bits, &mut buf, 134);
    buf
};
const LETTERS_CHARS: [char; 27] = [
    'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's',
    't', 'u', 'v', 'w', 'x', 'y', 'z', '?',
];

const fn str_to_bitmap(str: &str, fg: char, buf: &mut [bool]) {
    let bytes = str.as_bytes();
    let mut buf_index = 0;
    let mut i = 0;
    while i < bytes.len() {
        if bytes[i] != b'\n' {
            buf[buf_index] = bytes[i] == fg as u8;
            buf_index += 1;
        }

        i += 1;
    }
}

const fn bitmap_length(bits: &[bool], width: usize) -> usize {
    let mut count = 0;
    let mut x = 0;
    let mut x_start = 0;
    while x < width {
        let is_boundary = !bits[x]
            && !bits[width + x]
            && !bits[width * 2 + x]
            && !bits[width * 3 + x]
            && !bits[width * 4 + x]
            && !bits[width * 5 + x];

        if is_boundary && (x - x_start >= 3 && x - x_start <= 5) {
            count += 1;
        }

        if is_boundary {
            x_start = x;
        }

        x += 1;
    }

    count
}

const fn parse_bitmap(bits: &[bool], buffer: &mut [u32], width: usize) {
    let mut x_start = 0;
    let mut letter_index = 0;

    let mut x_end = 0;
    while x_end < width {
        let is_boundary = !bits[x_end]
            && !bits[width + x_end]
            && !bits[width * 2 + x_end]
            && !bits[width * 3 + x_end]
            && !bits[width * 4 + x_end]
            && !bits[width * 5 + x_end];

        if !is_boundary {
            x_end += 1;
            continue;
        }

        if x_end - x_start > 5 || x_end - x_start < 3 {
            x_end += 1;
            x_start = x_end;
            continue;
        }

        let mut bitmap = 0u32;

        let mut x = x_start;
        while x < x_end {
            bitmap <<= 6;
            bitmap |= (bits[x] as u32)
                | (bits[width + x] as u32) << 1
                | (bits[width * 2 + x] as u32) << 2
                | (bits[width * 3 + x] as u32) << 3
                | (bits[width * 4 + x] as u32) << 4
                | (bits[width * 5 + x] as u32) << 5;

            x += 1;
        }

        buffer[letter_index] = bitmap;

        letter_index += 1;
        x_end += 1;
        x_start = x_end;
    }
}

pub fn read_big_letters_string(str: &str, fg: char) -> String {
    let height = str.trim().lines().count();
    debug_assert_eq!(height, 6);
    let width = str
        .trim()
        .lines()
        .map(|l| l.len())
        .max()
        .unwrap_or_default();

    let mut bits = vec![false; width * height];
    str_to_bitmap(str, fg, &mut bits);

    read_bitmap_string(&bits, width)
}

pub fn read_bitmap_string(bits: &[bool], width: usize) -> String {
    let len = bitmap_length(bits, width);
    let mut letters = vec![0; len];
    parse_bitmap(bits, &mut letters, width);

    letters
        .iter()
        .map(|&w| LETTERS_CHARS[(0..26).find(|&i| LETTERS_BITMAPS[i] == w).unwrap_or(26)])
        .join("")
}
