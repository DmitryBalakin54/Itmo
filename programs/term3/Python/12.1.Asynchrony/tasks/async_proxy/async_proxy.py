import yarl
from aiohttp import web
import aiohttp


async def proxy_handler(request: web.Request) -> web.Response:
    """
    Check request contains http url in query args:
        /fetch?url=http%3A%2F%2Fexample.com%2F
    and trying to fetch it and return body with http status.
    If url passed without scheme or is invalid raise 400 Bad request.
    On failure raise 502 Bad gateway.
    :param request: aiohttp.web.Request to handle
    :return: aiohttp.web.Response
    """

    data = request.query.get('url')
    if not data:
        return web.Response(status=400, text='No url to fetch')

    url = yarl.URL(data)

    if not url.is_absolute():
        return web.Response(status=400, text='Empty url scheme')

    if not url.scheme == 'http':
        return web.Response(status=400, text=f'Bad url scheme: {url.scheme}')

    try:
        async with aiohttp.ClientSession() as session:
            async with session.get(url) as response:
                res = await response.text()
                if response.status == 500:
                    return web.Response(status=response.status, text='Internal server error')
                return web.Response(status=response.status, text=res)
    except aiohttp.ClientError:
        return web.Response(status=502, text='Bad Gateway')


async def setup_application(app: web.Application) -> None:
    """
    Setup application routes and aiohttp session for fetching
    :param app: app to apply settings with
    """

    session = aiohttp.ClientSession()
    app['session'] = session
    app.router.add_get('/fetch', proxy_handler)


async def teardown_application(app: web.Application) -> None:
    """
    Application with aiohttp session for tearing down
    :param app: app for tearing down
    """

    if 'session' in app:
        await app['session'].close()
