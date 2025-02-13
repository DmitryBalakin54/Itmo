package ru.itmo.wp.web.page;

import javax.servlet.http.HttpServletRequest;
import java.util.Map;

/** @noinspection unused*/
public class UsersPage extends Page {
    @Override
    protected void action(HttpServletRequest request, Map<String, Object> view) {
        if (getUser() != null) {
            view.put("isAdmin", getUser().getAdmin());
        } else {
            view.put("isAdmin", false);
        }
    }

    private void findAll(HttpServletRequest request, Map<String, Object> view) {
        view.put("users", userService.findAll());
    }

    private void findUser(HttpServletRequest request, Map<String, Object> view) {
        view.put("user",
                userService.find(Long.parseLong(request.getParameter("userId"))));
    }

    private void toggleAdminStatus(HttpServletRequest request, Map<String, Object> view) {
        if (getUser().getAdmin()) {
            long id = Long.parseLong(request.getParameter("userId"));
            boolean admin = Boolean.parseBoolean(request.getParameter("admin"));
            view.put("newAdminStatus", userService.toggleAdminStatusById(id, admin).getAdmin());
        }
    }
}

