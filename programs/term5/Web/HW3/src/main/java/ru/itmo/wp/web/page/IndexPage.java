package ru.itmo.wp.web.page;

import com.google.common.base.Strings;
import ru.itmo.wp.model.domain.User;

import javax.servlet.http.HttpServletRequest;
import java.util.Map;

@SuppressWarnings({"unused"})
public class IndexPage extends Page {
    private void findAll(HttpServletRequest request, Map<String, Object> view) {
        view.put("articles", articleService.findAllOrderedNotHidden());
    }

    private void findUser(HttpServletRequest request, Map<String, Object> view) {
        view.put("userLogin",
                userService.find(Long.parseLong(request.getParameter("userId"))).getLogin());
    }
}
