package ru.itmo.wp.controller;

import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import ru.itmo.wp.domain.User;
import ru.itmo.wp.service.UserService;

@Controller
@RequestMapping("/user")
public class UserPage extends Page {
    private final UserService userService;

    public UserPage(UserService userService) {
        this.userService = userService;
    }

    @GetMapping("/{userId}")
    public String user(@PathVariable String userId, Model model) {
        getMethodAction(model);
        User user = null;
        try {
            user = userService.findById(Long.parseLong(userId));
        } catch (NumberFormatException ignored) {}

        model.addAttribute("user_data", user);
        return getPage();
    }

    @GetMapping("")
    public String userEmpty(Model model) {
        getMethodAction(model);
        return getPage();
    }

    @Override
    protected String getPage() {
        return "UserPage";
    }
}
