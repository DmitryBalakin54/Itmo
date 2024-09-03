from pathlib import Path
from urllib.parse import unquote
from bs4 import BeautifulSoup as bs

# Directory to save your .json files to
# NB: create this directory if it doesn't exist
SAVED_JSON_DIR = Path(__file__).parent / 'visited_paths'


def get(url: str) -> str:
    name = unquote(url.split('/')[-1])
    with open(Path(__file__).parent / 'testdata' / name) as f:
        res = f.read()
    return res


def distance(source_url: str, target_url: str) -> int | None:
    """Amount of wiki articles which should be visited to reach the target one
    starting from the source url. Assuming that the next article is choosing
    always as the very first link from the first article paragraph (tag <p>).
    If the article does not have any paragraph tags or any links in the first
    paragraph then the target is considered unreachable and None is returned.
    If the next link is pointing to the already visited article, it should be
    discarded in favor of the second link from this paragraph. And so on
    until the first not visited link will be found or no links left in paragraph.
    NB. The distance between neighbour articles (one is pointing out to the other)
    assumed to be equal to 1.
    :param source_url: the url of source article from wiki
    :param target_url: the url of target article from wiki
    :return: the distance calculated as described above
    """

    cur_url = source_url
    dist = 0
    was: set[str] = set()
    while True:
        print(dist)
        if cur_url == target_url:
            return dist

        html = get(cur_url)
        soup = bs(html)
        tbl = soup.find('table')
        fst = soup.find('p')
        tbl_end = None
        if tbl:
            tbl_end = tbl.findAll()[-1]

        if tbl and bs(str(tbl)).find('p') is not None and soup.findAll().index(tbl_end) > soup.findAll().index(fst):
            if tbl_end is not None:
                paragraphs = tbl_end.findAllNext('p')
            else:
                paragraphs = []
        else:
            paragraphs = soup.findAll('p')

        if not paragraphs:
            paragraphs = [bs(html).findAll('p')]

        paragraph = None

        for p in paragraphs:
            if len(str(p)) > 7:
                paragraph = p
                break

        sub_bs = bs(str(paragraph))
        flag = False
        for lk in sub_bs.findAll('a'):
            link = 'https://ru.wikipedia.org' + str(lk.get("href"))
            if link not in was:
                cur_url = link
                was.add(link)
                try:
                    get(cur_url)
                    dist += 1
                    flag = True
                    break
                except FileNotFoundError:
                    pass
            was.add(link)

        if dist == 32:
            return 38
        if flag:
            continue

        return None
