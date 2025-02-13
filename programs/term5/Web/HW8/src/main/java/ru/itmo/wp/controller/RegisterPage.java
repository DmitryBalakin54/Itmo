package ru.itmo.wp.controller;

import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.validation.BindingResult;
import org.springframework.web.bind.WebDataBinder;
import org.springframework.web.bind.annotation.*;
import ru.itmo.wp.form.UserCredentials;
import ru.itmo.wp.form.validator.UserCredentialsRegisterValidator;
import ru.itmo.wp.service.UserService;

import javax.servlet.http.HttpSession;
import javax.validation.Valid;
import java.util.Map;

@Controller
@RequestMapping("/register")
public class RegisterPage extends Page {
    private final UserService userService;
    private final UserCredentialsRegisterValidator userCredentialsRegisterValidator;

    public RegisterPage(UserService userService, UserCredentialsRegisterValidator userCredentialsRegisterValidator) {
        this.userService = userService;
        this.userCredentialsRegisterValidator = userCredentialsRegisterValidator;
    }

    @InitBinder
    public void initBinder(WebDataBinder binder) {
        if (binder.getTarget() instanceof UserCredentials) {
            binder.addValidators(userCredentialsRegisterValidator);
        }
    }

    @GetMapping("")
    public String registerGet(Model model) {
        getMethodAction(model);
        model.addAttribute("registerForm", new UserCredentials());
        return getPage();
    }

    @PostMapping("")
    public String registerPost(@Valid @ModelAttribute("registerForm") UserCredentials registerForm,
                               BindingResult bindingResult,
                               HttpSession httpSession) {
        if (bindingResult.hasErrors()) {
            return getPage();
        }

        setUser(httpSession, userService.register(registerForm));
        setMessage(httpSession, "Congrats, you have been registered!");

        return "redirect:/";
    }

    @Override
    protected String getPage() {
        return "RegisterPage";
    }
}
