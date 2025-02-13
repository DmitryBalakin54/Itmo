package ru.itmo.wp.model.service;

import com.google.common.base.Strings;
import ru.itmo.wp.model.domain.Article;
import ru.itmo.wp.model.exception.ValidationException;
import ru.itmo.wp.model.repository.ArticleRepository;
import ru.itmo.wp.model.repository.impl.ArticleRepositoryImp;

import java.util.List;

public class ArticleService {
    private ArticleRepository articleRepository = new ArticleRepositoryImp();

    public void add(Article article) {
        articleRepository.save(article);
    }

    public Article find(long id) {
        return articleRepository.find(id);
    }

    public List<Article> findAll() {
        return articleRepository.findAll();
    }

    public List<Article> findAllByUserid(long userid) {
        return articleRepository.findAllByUserId(userid);
    }

    public void validate(Article article) throws ValidationException {
        if (Strings.isNullOrEmpty(article.getTitle()) || Strings.isNullOrEmpty(article.getText())) {
            throw new ValidationException("Title or text are required");
        }
    }

    public List<Article> findAllOrdered() {
        return articleRepository.findAllOrdered();
    }

    public List<Article> findAllOrderedChronology() {
        return articleRepository.findAllOrderedChronology();
    }

    public List<Article> findAllOrderedNotHidden() {
        return articleRepository.findAllOrderedNotHidden();
    }

    public Article toggleHidden(long id, boolean hidden, long userId) {
        if (userId != articleRepository.find(id).getUserId()) {
            return null;
        }

        return articleRepository.toggleHiddenById(id, hidden);
    }

    public List<Article> findAllOrderedChronologyByUserId(long userId) {
        return articleRepository.findAllOrderedChronologyByUserId(userId);
    }
}
