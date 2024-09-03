import argparse
import socket
import sys
from pathlib import Path

FILE_DIR = Path(__file__).parent


def ans_word(was: set[str], words: dict[str, list[str]], letter: str) -> str | None:
    for w in words[letter]:
        if w not in was:
            return w
    return None


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument('--port', default=59898)
    args = parser.parse_args(sys.argv[1:])

    s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    words: dict[str, list[str]] = {}

    with open(f'{FILE_DIR}/words.txt', 'r') as f:
        wrds = f.read().split('\n')
        for w in wrds:
            if len(w) > 3:
                if w[0] in words:
                    words[w[0]].append(w)
                else:
                    words[w[0]] = []
                    words[w[0]].append(w)

    was: set[str] = set()
    s.connect(('localhost', int(args.port)))
    fl = s.makefile('r')
    try:
        while True:
            msg = fl.readline()
            line = msg
            print(line)
            if line.startswith('WELCOME') or line.startswith('MESSAGE') or line.startswith('VALID'):
                continue
            elif line.startswith('INVALID') or line.startswith('PLAYER_DEFEAT'):
                s.close()
                fl.close()
                raise Exception('fuck')
            elif line.startswith('PLAYER_VICTORY'):
                break
            elif line.startswith('PLAY'):
                word = line.split(' ')[1][:-1]
                was.add(word)
                new_word = ans_word(was, words, word[-1])
                if new_word is None:
                    print('SURRENDER')
                    s.send(b'SURRENDER\n')
                    continue

                was.add(new_word)
                res = f'PLAY {new_word}\n'
                print(res, end='')
                s.send(res.encode())
            else:
                break
    except KeyboardInterrupt:
        s.send(b'QUIT\n')
        print('QUIT')
    finally:
        s.close()
        fl.close()


if __name__ == '__main__':
    main()
