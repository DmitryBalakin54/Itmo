package ru.itmo.wp.web.page;

import ru.itmo.wp.model.domain.User;
import ru.itmo.wp.web.exception.RedirectException;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.util.Map;

@SuppressWarnings({"unused"})
public class LogoutPage extends Page {

    @Override
    protected void action(HttpServletRequest request, Map<String, Object> view) {
        request.getSession().removeAttribute("user");

//        request.getSession().setAttribute("message", "Good bye. Hope to see you soon!");
        setMessage("Good bye. Hope to see you soon!");
        throw new RedirectException("/index");
    }

    @Override
    protected void before(HttpServletRequest request, Map<String, Object> view) {
        super.before(request, view);
        User user = getUser();
        setLogoutEvent(user);
    }
}
