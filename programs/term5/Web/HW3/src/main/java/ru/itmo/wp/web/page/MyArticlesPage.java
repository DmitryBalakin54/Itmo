package ru.itmo.wp.web.page;

import ru.itmo.wp.web.exception.RedirectException;

import javax.servlet.http.HttpServletRequest;
import java.util.Map;

public class MyArticlesPage extends Page {

    @Override
    protected void action(HttpServletRequest request, Map<String, Object> view) {
        if (getUser() == null) {
            setMessage("You must be registered to see yours articles.");
            throw new RedirectException("/index");
        }

        view.put("articles", articleService.findAllOrderedChronologyByUserId(getUser().getId()));
    }

    private void toggleHidden(HttpServletRequest request, Map<String, Object> view) {
        long id = Long.parseLong(request.getParameter("id"));
        boolean hidden = Boolean.parseBoolean(request.getParameter("hidden"));
        long userId = Long.parseLong(request.getParameter("userId"));

        view.put("newArticle", articleService.toggleHidden(id, hidden, userId));
    }
}
