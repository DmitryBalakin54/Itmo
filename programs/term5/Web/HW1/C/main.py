import requests

rr = []
rh = []
url = 'http://1d3p.wp.codeforces.com/new'

headers = {
    'Accept': 'text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.7',
    'Accept-Encoding': 'gzip, deflate',
    'Accept-Language': 'ru-RU,ru;q=0.9,en-US;q=0.8,en;q=0.7',
    'Cache-Control': 'max-age=0',
    'Connection': 'keep-alive',
    'Content-Length': '54',
    'Content-Type': 'application/x-www-form-urlencoded',
    'Host': '1d3p.wp.codeforces.com',
    'Origin': 'http://1d3p.wp.codeforces.com',
    'Referer': 'http://1d3p.wp.codeforces.com/',
    'Upgrade-Insecure-Requests': '1',
    'User-Agent': 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/128.0.0.0 Safari/53'
}

cookies = {
    '70a7c28f3de': '0n1slxyp9deiwtbqxk',
    '_gid': 'GA1.2.2040323424.1726611285',
    '_ga_K230KVN22K': 'GS1.1.1726614524.9.1.1726619830.0.0.0',
    '_ga': 'GA1.1.623323705.1700507690',
    'JSESSIONID': '8932DFA85D25CB0B0F08768DC08E5C5A'
}

for i in range(1, 101):
    data = {
        '_af': '34be50b38beccce4',
        'proof': i ^ 2,
        'amount': i,
        'submit': 'Submit'
    }

    response = requests.post(url, verify=False, data=data, headers=headers, cookies=cookies)

    print(response.status_code)
    rr = response.text
    rh = response.headers

print(rr)
print(rh)
