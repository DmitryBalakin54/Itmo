package ru.itmo.wp.web.page;

import com.google.common.base.Strings;
import ru.itmo.wp.model.domain.User;
import ru.itmo.wp.model.service.ArticleService;
import ru.itmo.wp.model.service.UserService;

import javax.servlet.http.HttpServletRequest;
import java.util.Map;

public class Page {
    protected final UserService userService = new UserService();
    protected final ArticleService articleService = new ArticleService();

    protected HttpServletRequest request;

    protected void action(HttpServletRequest request, Map<String, Object> view) {
        // No operations.
    }

    protected void before(HttpServletRequest request, Map<String, Object> view) {
        this.request = request;

        putUser(view);
        putMessage(view);
    }

    protected void after(HttpServletRequest request, Map<String, Object> view) {

    }

    private void putUser(Map<String, Object> view) {
        User user = getUser();
        if (user != null) {
            view.put("user", user);
        }
    }

    protected final void putMessage(Map<String, Object> view) {
        String message = (String) request.getSession().getAttribute("message");
        if (!Strings.isNullOrEmpty(message)) {
            view.put("message", message);
            request.getSession().removeAttribute("message");
        }
    }

    protected final void setMessage(String message) {
        request.getSession().setAttribute("message", message);
    }

    protected final void setUser(User user) {
        request.getSession().setAttribute("user", user);
    }

    protected final User getUser() {
        User user = (User) request.getSession().getAttribute("user");

        return user;
    }
}
