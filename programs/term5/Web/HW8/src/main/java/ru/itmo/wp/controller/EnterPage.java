package ru.itmo.wp.controller;

import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.validation.BindingResult;
import org.springframework.web.bind.WebDataBinder;
import org.springframework.web.bind.annotation.*;
import ru.itmo.wp.form.UserCredentials;
import ru.itmo.wp.form.validator.UserCredentialsEnterValidator;
import ru.itmo.wp.service.UserService;

import javax.servlet.http.HttpSession;
import javax.validation.Valid;

@Controller
@RequestMapping("/enter")
public class EnterPage extends Page {
    private final UserService userService;
    private final UserCredentialsEnterValidator userCredentialsEnterValidator;

    public EnterPage(UserService userService, UserCredentialsEnterValidator userCredentialsEnterValidator) {
        this.userService = userService;
        this.userCredentialsEnterValidator = userCredentialsEnterValidator;
    }

    @InitBinder
    public void initBinderPage(WebDataBinder binder) {
        if (binder.getTarget() instanceof UserCredentials) {
            binder.addValidators(userCredentialsEnterValidator);
        }
    }

    @Override
    protected String getPage() {
        return "EnterPage";
    }

    @GetMapping("")
    public String register(Model model) {
        getMethodAction(model);
        model.addAttribute("enterForm", new UserCredentials());
        return getPage();
    }

    @PostMapping("")
    public String register(@Valid @ModelAttribute("enterForm") UserCredentials enterForm,
                           BindingResult bindingResult,
                           HttpSession httpSession) {
        if (bindingResult.hasErrors()) {
            return getPage();
        }

        setUser(httpSession, userService.findByLoginAndPassword(enterForm.getLogin(), enterForm.getPassword()));
        setMessage(httpSession, "Hello, " + getUser(httpSession).getLogin());

        return "redirect:";
    }
}
