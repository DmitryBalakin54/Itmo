from bs4 import BeautifulSoup


class GameInfo:
    def __init__(self):
        self.title = ''
        self.views = ''
        self.downloads = ''
        self.release_date = ''
        self.genres = ''
        self.publication_type = ''
        self.interface_languages = ''
        self.voice_languages = ''
        self.tablet = ''
        self.min_hdd_space = ''

    @staticmethod
    def get_tsv_str_header() -> str:
        return ("title\trelease_date\tviews\tdownloads\tgenres\tpublication_type\tinterface_languages\tvoice_"
                "languages\ttablet\tmin_hdd_space(Mb)")

    def normalise(self):
        self.title = self.title.lower()
        self.views = self.views.lower()
        self.downloads = self.downloads.lower()
        self.release_date = self.release_date.lower()
        self.genres = self.genres.lower()
        self.publication_type = self.publication_type.lower()
        self.interface_languages = self.interface_languages.lower()
        self.voice_languages = self.voice_languages.lower()
        self.tablet = self.tablet.lower()
        self.min_hdd_space = self.normalise_hdd_space().lower()

    def normalise_hdd_space(self):
        hdd = self.min_hdd_space.lower()
        if '(' in hdd:
            hdd = hdd[:hdd.find('(')]
        if len(hdd) == 0:
            return ''
        sep = ''
        mult = 1
        if 'гб' in hdd:
            sep = 'гб'
            mult = 1024
        elif 'gb' in hdd:
            sep = 'gb'
            mult = 1024
        elif 'мб' in hdd:
            sep = 'мб'
        elif 'mb' in hdd:
            sep = 'mb'
        else:
            res = hdd.replace(',', '.').strip()
            return str(int(float(res) * 1024))

        hdd = hdd[:hdd.find(sep)].replace(',', '.').strip()
        if '-' in hdd:
            hdd = hdd.split('-')[1]
        return str(int(float(hdd) * mult))

    def get_tsv_str(self):
        res = [self.title,
               self.release_date,
               self.views,
               self.downloads,
               self.genres,
               self.publication_type,
               self.interface_languages,
               self.voice_languages,
               self.tablet,
               self.min_hdd_space]
        return '\t'.join(['"' + s + '"' for s in res])


def norm(s):
    return s.strip().replace('\t', ' ').replace(' ', ' ')


with open('raw_data.tsv', 'w', encoding='utf-8') as file:
    info = open('pages/info', 'r', encoding='utf-8')
    cnt = int(info.readline())
    info.close()
    file.write(GameInfo.get_tsv_str_header())
    file.write('\n')
    for i in range(1, cnt + 1):
        with open(f'pages/{i}.html', 'r', encoding='utf-8') as game:
            current_game_info = GameInfo()
            game_soup = BeautifulSoup(game, 'html.parser')

            current_game_info.title = norm(game_soup.find('h1', 'inner-entry__title').text).replace(',', ' ')
            current_game_info.release_date = norm(game_soup.find('span', class_='entry__date').text)
            views = norm(game_soup.find('span', class_='entry__reads').text)
            current_game_info.views = norm(''.join(filter(lambda ch: ch.isdigit(), views)))

            downloads = game_soup.find('span', {'id': 'loads'})
            if downloads is None:
                current_game_info.downloads = ''
            else:
                current_game_info.downloads = norm(downloads.text)

            game_details = game_soup.find('div', class_='inner-entry__details').find_all('strong')
            current_game_info.genres = norm(', '.join(
                [i.text for i in game_soup.find('div', class_='inner-entry__details').find_all('a')]))
            current_game_info.publication_type = norm(game_details[4].next_sibling.text)
            current_game_info.interface_languages = norm(game_details[5].next_sibling.text)
            current_game_info.voice_languages = norm(game_details[6].next_sibling.text)
            current_game_info.tablet = norm(game_details[7].next_sibling.text)

            min_params = game_soup.find('ul', class_='inner-entry__sysreq-list').find_all('strong')
            current_game_info.min_hdd_space = norm(min_params[4].next_sibling.text)
            current_game_info.normalise()
            file.write(current_game_info.get_tsv_str())
            file.write('\n')
