import typing

import pytest

from .banner_engine import (
    BannerStat, Banner, EpsilonGreedyBannerEngine, EmptyBannerStorageError, BannerStorage
)

TEST_DEFAULT_CTR = 0.1


@pytest.fixture(scope="function")
def test_banners() -> list[Banner]:
    return [
        Banner("b1", cost=1, stat=BannerStat(10, 20)),
        Banner("b2", cost=250, stat=BannerStat(20, 20)),
        Banner("b3", cost=100, stat=BannerStat(0, 20)),
        Banner("b4", cost=100, stat=BannerStat(1, 20)),
    ]


@pytest.mark.parametrize("clicks, shows, expected_ctr", [(1, 1, 1.0), (20, 100, 0.2), (5, 100, 0.05)])
def test_banner_stat_ctr_value(clicks: int, shows: int, expected_ctr: float) -> None:
    assert BannerStat(clicks, shows).compute_ctr(TEST_DEFAULT_CTR) == expected_ctr


@pytest.mark.parametrize("clicks, shows", [(1, 0), (20, 0), (5, 0), (0, 0), (1, 2)])
def test_empty_stat_compute_ctr_returns_default_ctr(clicks: int, shows: int) -> None:
    if shows == 0:
        assert BannerStat(clicks, shows).compute_ctr(TEST_DEFAULT_CTR) == TEST_DEFAULT_CTR


@pytest.mark.parametrize("clicks, shows", [(20, 5), (5, 8), (0, 5)])
def test_banner_stat_add_show_lowers_ctr(clicks: int, shows: int) -> None:
    bs = BannerStat(clicks, shows)
    last = bs.compute_ctr(TEST_DEFAULT_CTR)
    bs.add_show()
    assert last >= bs.compute_ctr(TEST_DEFAULT_CTR)


def test_banner_stat_add_click_increases_ctr() -> None:
    pass


@pytest.mark.parametrize("test_banners, expected",
                         [([Banner("1", 1, BannerStat(3, 2)), Banner("2", 1, BannerStat(2, 3))], "1"),
                          ([Banner("1", 0, BannerStat(3, 2)), Banner("2", 100, BannerStat(10, 0))], "2"),
                          ([Banner("1", 44, BannerStat(3, 2)), Banner("2", 25, BannerStat(2, 3)),
                            Banner("3", 10000, BannerStat(1, 1))], "3")])
def test_get_banner_with_highest_cpc_returns_banner_with_highest_cpc(test_banners: list[Banner], expected) -> None:
    assert BannerStorage(test_banners).banner_with_highest_cpc().banner_id == expected


@pytest.mark.parametrize("banners", [[]])
def test_banner_engine_raise_empty_storage_exception_if_constructed_with_empty_storage(banners) -> None:
    with pytest.raises(EmptyBannerStorageError):
        EpsilonGreedyBannerEngine(BannerStorage(banners), 0)


def test_engine_send_click_not_fails_on_unknown_banner(test_banners: list[Banner]) -> None:
    pass


def test_engine_with_zero_random_probability_shows_banner_with_highest_cpc(test_banners: list[Banner]) -> None:
    pass


@pytest.mark.parametrize("expected_random_banner", ["b1", "b2", "b3", "b4"])
def test_engine_with_1_random_banner_probability_gets_random_banner(
        expected_random_banner: str,
        test_banners: list[Banner],
        monkeypatch: typing.Any
) -> None:
    pass


def test_total_cost_equals_to_cost_of_clicked_banners(test_banners: list[Banner]) -> None:
    pass


def test_engine_show_increases_banner_show_stat(test_banners: list[Banner]) -> None:
    pass


def test_engine_click_increases_banner_click_stat(test_banners: list[Banner]) -> None:
    pass
