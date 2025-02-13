set url=http://1d3p.wp.codeforces.com/new

set headers=^
 -H "Accept: text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.7"^
 -H "Accept-Encoding: gzip, deflate"^
 -H "Accept-Language: ru-RU,ru;q=0.9,en-US;q=0.8,en;q=0.7"^
 -H "Cache-Control: max-age=0"^
 -H "Connection: keep-alive"^
 -H "Content-Type: application/x-www-form-urlencoded"^
 -H "Origin: http://1d3p.wp.codeforces.com"^
 -H "Referer: http://1d3p.wp.codeforces.com/"^
 -H "Upgrade-Insecure-Requests: 1"^
 -H "User-Agent: Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/128.0.0.0 Safari/53"

set cookies=^
 -b "70a7c28f3de=0n1slxyp9deiwtbqxk"^
 -b "_gid=GA1.2.2040323424.1726611285"^
 -b "_ga_K230KVN22K=GS1.1.1726614524.9.1.1726619830.0.0.0"^
 -b "_ga=GA1.1.623323705.1700507690"^
 -b "JSESSIONID=8932DFA85D25CB0B0F08768DC08E5C5A"

for /L %%i in (1,1,100) do (
    set /a proof=%%i*%%i
    call curl -X POST %url% %headers% %cookies% ^ 
     --data-urlencode "_af=34be50b38beccce4" ^ 
     --data-urlencode "proof=%%proof%%" ^ 
     --data-urlencode "amount=%%i" ^ 
     --data-urlencode "submit=Submit" ^ 
     --insecure
)
