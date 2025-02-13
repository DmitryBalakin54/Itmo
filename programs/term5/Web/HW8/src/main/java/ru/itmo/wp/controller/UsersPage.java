package ru.itmo.wp.controller;

import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import ru.itmo.wp.domain.User;
import ru.itmo.wp.form.UserToggleDisabledForm;
import ru.itmo.wp.service.UserService;

import javax.servlet.http.HttpSession;
import javax.validation.Valid;

@Controller
@RequestMapping("/users")
public class UsersPage extends Page {
    private final UserService userService;
    private final HttpSession httpSession;

    public UsersPage(UserService userService, HttpSession httpSession) {
        this.userService = userService;
        this.httpSession = httpSession;
    }

    @GetMapping("/all")
    public String users(Model model) {
        getMethodAction(model);
        model.addAttribute("users", userService.findAll());
        return getPage();
    }

    @PostMapping("/all")
    public String toggleDisabled(@Valid @ModelAttribute UserToggleDisabledForm userForm) {
        User user = userService.findById(userForm.getId());
        user.setDisabled(userForm.getDisabled());
        if (user.equals(getUser(httpSession))) {
            setMessageError(httpSession, "You cant change your status");
            return "redirect:/users/all";

        }
        userService.save(user);
        return "redirect:/users/all";
    }

    @GetMapping("")
    public String redirectToAll() {
        return "redirect:/users/all";
    }

    @Override
    protected String getPage() {
        return "UsersPage";
    }
}
