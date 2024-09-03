import asyncio
import aiohttp
import requests
import threading


async def async_fetch(session: aiohttp.ClientSession, url: str) -> str:
    """
    Asyncronously fetch (get-request) single url using provided session
    :param session: aiohttp session object
    :param url: target http url
    :return: fetched text
    """

    async with session.get(url) as req:
        res = await req.text()
    return res


async def async_requests(urls: list[str]) -> list[str]:
    """
    Concurrently fetch provided urls using aiohttp
    :param urls: list of http urls ot fetch
    :return: list of fetched texts
    """
    async with aiohttp.ClientSession() as session:
        res = list(await asyncio.gather(*[async_fetch(session, url) for url in urls]))

    return res


def sync_fetch(session: requests.Session, url: str) -> str:
    """
    Syncronously fetch (get-request) single url using provided session
    :param session: requests session object
    :param url: target http url
    :return: fetched text
    """

    return session.get(url).text


def threaded_requests(urls: list[str]) -> list[str]:
    """
    Concurrently fetch provided urls with requests in different threads
    :param urls: list of http urls ot fetch
    :return: list of fetched texts
    """
    def f(s: requests.Session, u: str, r: list[str], ind: int) -> None:
        r[ind] = sync_fetch(s, u)

    res = [''] * len(urls)
    with requests.session() as session:
        threads = [threading.Thread(target=f, args=(session, url, res, i)) for i, url in enumerate(urls)]
        for thread in threads:
            thread.start()
        for thread in threads:
            thread.join()

    return res
