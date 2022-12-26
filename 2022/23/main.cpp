#include <cstddef>
#include <curl/curl.h>
#include <fstream>
#include <iostream>
#include <iterator>
#include <limits>
#include <math.h>
#include <optional>
#include <sstream>
#include <unordered_map>
#include <vector>

using std::string;

size_t write_callback(char *ptr, size_t size, size_t nmemb, std::string *buf) {
    const size_t length = size * nmemb;
    buf->insert(buf->end(), ptr, ptr + length);
    return length;
}

string fetch_input() {
    string        cookie;
    std::ifstream session("../../session");
    std::getline(session, cookie);
    cookie.insert(0, "session=");

    curl_global_init(CURL_GLOBAL_ALL);
    CURL *curl = curl_easy_init();

    if (!curl) {
        std::cerr << "Couldn't initialize curl\n";
        exit(1);
    }

    std::string buf;

    curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, write_callback);
    curl_easy_setopt(curl, CURLOPT_WRITEDATA, &buf);
    curl_easy_setopt(curl, CURLOPT_COOKIE, cookie.c_str());
    curl_easy_setopt(curl, CURLOPT_URL, "https://adventofcode.com/2022/day/23/input");

    CURLcode res = curl_easy_perform(curl);
    if (res != CURLE_OK) {
        std::cerr << "Request Response Code was not OK\n";
        exit(1);
    }

    return buf;
}

string get_input() {
    std::ifstream input_file("input");

    if (input_file.is_open()) {
        std::stringstream buffer;
        buffer << input_file.rdbuf();
        return buffer.str();
    } else {
        string        input = fetch_input();
        std::ofstream input_file("input");
        input_file << input;
        return input;
    }
}

template <class T> inline void hash_combine(std::size_t &seed, const T &v) {
    std::hash<T> hasher;
    seed ^= hasher(v) + 0x9e3779b9 + (seed << 6) + (seed >> 2);
}

class vec2 {
  public:
    int x, y;

    vec2(int vx, int vy) {
        x = vx;
        y = vy;
    }

    inline vec2 operator+(const vec2 &o) const { return vec2(x + o.x, y + o.y); }
    inline vec2 operator+(const int &o) const { return vec2(x + o, y + o); }
    inline vec2 operator/(const int &o) const { return vec2(x / o, y / o); }
    friend bool operator==(const vec2 &a, const vec2 &b);
};

inline bool operator==(const vec2 &a, const vec2 &b) { return a.x == b.x && a.y == b.y; }

template <> struct std::hash<vec2> {
    std::size_t operator()(vec2 const &s) const noexcept {
        std::size_t h1 = std::hash<int>{}(s.x);
        std::size_t h2 = std::hash<int>{}(s.y);
        return h1 ^= h2 + 0x9e3779b9 + (h1 << 6) + (h1 >> 2);
    }
};

class BoundingBox {
  public:
    int x0, y0, x1, y1;

    BoundingBox() {
        x0 = INT_MAX;
        y0 = INT_MAX;
        x1 = INT_MIN;
        y1 = INT_MIN;
    }

    void extend(const vec2 p) {
        x0 = std::min(x0, p.x);
        y0 = std::min(y0, p.y);
        x1 = std::max(x1, p.x);
        y1 = std::max(y1, p.y);
    }

    inline int width() const { return x1 - x0 + 1; }

    inline int height() const { return y1 - y0 + 1; }

    inline int area() const { return width() * height(); }
};

class Elf {
  public:
    std::optional<vec2> proposal;
    Elf() { proposal = std::nullopt; }
};

const vec2 neighbours_offset[8] = {
    vec2(-1, -1), vec2(0, -1), vec2(1, -1), vec2(-1, 0), vec2(1, 0), vec2(-1, 1), vec2(0, 1), vec2(1, 1),
};

class State {
  private:
    std::unordered_map<vec2, Elf>      data;
    std::unordered_map<vec2, int>      proposals;
    std::vector<std::pair<vec2, vec2>> moves;

    vec2 directions[4][3] = {
        {vec2(0, -1), vec2(-1, -1), vec2(1, -1)},
        {vec2(0, 1), vec2(-1, 1), vec2(1, 1)},
        {vec2(-1, 0), vec2(-1, -1), vec2(-1, 1)},
        {vec2(1, 0), vec2(1, -1), vec2(1, 1)},
    };

    template <class input_iterator> int count_elf(vec2 pos, input_iterator it, input_iterator end) {
        int count = 0;
        while (it != end) {
            if (data.contains(*(it++) + pos))
                count++;
        }
        return count;
    }

  public:
    int elf_count   = 0;
    int round_count = 0;

    State(string const &input) {
        vec2 pos(0, 0);
        for (size_t i = 0; i < input.length(); i++) {
            char c = input[i];
            if (c == '\n') {
                pos.x = 0;
                pos.y++;
                continue;
            }

            if (c == '#') {
                data.emplace(pos, Elf());
                elf_count++;
            }

            pos.x++;
        }

        moves.reserve(elf_count);
    }

    BoundingBox bounding_box() {
        BoundingBox bbox;

        for (auto const &[pos, _] : data) {
            bbox.extend(pos);
        }

        return bbox;
    }

    int round() {
        proposals.clear();
        // First half
        for (auto &[pos, elf] : data) {
            int neighbour_count = count_elf(pos, neighbours_offset, neighbours_offset + 8);
            elf.proposal        = std::nullopt;

            if (neighbour_count == 0) {
                // Elf has no dirrect neighbour, do nothing
                continue;
            }

            for (auto const offs : directions) {
                int count = count_elf(pos, offs, offs + 3);
                if (count > 0) {
                    continue;
                }

                vec2 prop    = pos + offs[0];
                elf.proposal = prop;
                proposals.try_emplace(prop, 0);
                proposals[prop]++;
                break;
            }
        }

        moves.clear();
        // Second half
        for (auto &[pos, elf] : data) {
            if (!elf.proposal.has_value()) {
                continue;
            }

            vec2 prop = elf.proposal.value();
            if (proposals[prop] > 1) {
                // More than one elf wants to go to that tile, so none of them go.
                continue;
            }

            moves.push_back(std::make_pair(pos, prop));
        }

        for (auto const &[from, to] : moves) {
            auto nh  = data.extract(from);
            nh.key() = to;
            data.insert(std::move(nh));
        }

        // rotate (since std::rotate doesn't work because vec2[3] is not assignable)
        {
            vec2 first[3] = {directions[0][0], directions[0][1], directions[0][2]};
            for (int i = 0; i < 3; i++) {
                directions[i][0] = directions[i + 1][0];
                directions[i][1] = directions[i + 1][1];
                directions[i][2] = directions[i + 1][2];
            }
            directions[3][0] = first[0];
            directions[3][1] = first[1];
            directions[3][2] = first[2];
        }

        round_count++;
        return moves.size();
    }

    void show() {
        const auto bbox = bounding_box();
        const auto w    = bbox.width();
        const auto h    = bbox.height();
        string     buf((w * 2 + 1) * h, ' ');

        for (size_t i = w * 2; i < buf.length(); i += w * 2 + 1)
            buf[i] = '\n';
        for (int y = 0; y < h; y++)
            for (int x = 0; x < w * 2; x += 2)
                buf[y * (w * 2 + 1) + x] = '.';
        for (const auto &[pos, _] : data) {
            int i      = (pos.y - bbox.y0) * (w * 2 + 1) + (pos.x - bbox.x0) * 2;
            buf[i]     = '[';
            buf[i + 1] = ']';
        }

        std::cout << "\033[;H\033[2Jround " << round_count << "\n" << buf;
    }

    int empty_count() { return bounding_box().area() - elf_count; }
};

int solve_part1(const string &input) {
    State state(input);
    int   r = 10;
    while (r-- > 0) {
        state.round();
    }

    return state.empty_count();
}

int solve_part2(const string &input) {
    State state(input);
    while (state.round() > 0)
        ;
    return state.round_count;
}

int main() {
    string input = get_input();

    int part1 = solve_part1(input);
    int part2 = solve_part2(input);

    std::cout << "Part1: " << part1 << std::endl;
    std::cout << "Part2: " << part2 << std::endl;

    return 0;
}
