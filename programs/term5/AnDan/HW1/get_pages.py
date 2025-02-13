import requests
from bs4 import BeautifulSoup

url = 'https://byxatab.com/torrent_igry/rpg/'

main_page = requests.get(url)
soup = BeautifulSoup(main_page.text, 'html.parser')

bottom_nav = soup.find('div', {'id': 'bottom-nav'})
last_page_num = int(bottom_nav.find_all('a')[-1].text)
cnt = 0

for num in range(1, last_page_num + 1):
    current_page_url = url + f'page/{num}/'
    current_page = requests.get(current_page_url)
    current_page_soup = BeautifulSoup(current_page.text, 'html.parser')
    current_games = current_page_soup.find_all('div', class_='entry__title h2')
    for game in current_games:
        game_page_href = game.find('a')['href']
        game_page = requests.get(game_page_href)
        cnt += 1
        with open(f'pages/{cnt}.html', 'w', encoding='utf-8') as file:
            file.write(game_page.text)


print(cnt)
with open(f'pages/info', 'w', encoding='utf-8') as file:
    file.write(str(cnt))