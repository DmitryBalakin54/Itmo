package ru.itmo.wp.model.repository;

import ru.itmo.wp.model.domain.Article;

import java.util.List;

public interface ArticleRepository {
    Article find(long id);

    List<Article> findAllByUserId(long userId);

    void save(Article article);

    List<Article> findAll();

    List<Article> findAllOrdered();

    List<Article> findAllOrderedChronology();

    List<Article> findAllOrderedChronologyByUserId(long userId);

    List<Article> findAllOrderedNotHidden();

    Article toggleHiddenById(long id, boolean hidden);
}
