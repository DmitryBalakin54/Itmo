package ru.itmo.wp.web.page;

import ru.itmo.wp.model.domain.Article;
import ru.itmo.wp.model.domain.User;
import ru.itmo.wp.model.exception.ValidationException;
import ru.itmo.wp.model.service.ArticleService;
import ru.itmo.wp.model.service.UserService;
import ru.itmo.wp.web.exception.RedirectException;

import javax.servlet.http.HttpServletRequest;
import java.util.Map;

public class ArticlePage extends Page {

    @Override
    protected void action(HttpServletRequest request, Map<String, Object> view) {
        if (getUser() == null) {
            setMessage("You must be registered to write an article.");
            throw new RedirectException("/index");
        }
    }

    private void create(HttpServletRequest request, Map<String, Object> view) throws ValidationException {
        Article article = new Article();
        article.setTitle(request.getParameter("title"));
        article.setText(request.getParameter("text"));
        article.setHidden(request.getParameter("hidden").equals("true"));
        article.setUserId(getUser().getId());

        articleService.validate(article);
        articleService.add(article);

        request.getSession().setAttribute("message", "Article has been created");
        throw new RedirectException("/article");
    }
}
