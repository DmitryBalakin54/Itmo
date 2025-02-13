package ru.itmo.wp.controller;

import org.springframework.validation.BindingResult;
import org.springframework.web.bind.WebDataBinder;
import org.springframework.web.bind.annotation.*;
import ru.itmo.wp.domain.User;
import ru.itmo.wp.exception.ValidationException;
import ru.itmo.wp.form.UserCredentials;
import ru.itmo.wp.form.validator.UserCredentialsEnterValidator;
import ru.itmo.wp.service.JwtService;
import ru.itmo.wp.service.UserService;

import javax.validation.Valid;

@RestController
@RequestMapping("/api")
public class JwtController {

    private final UserService userService;
    private final JwtService jwtService;

    public JwtController(UserService userService, JwtService jwtService, UserCredentialsEnterValidator enterValidator) {
        this.userService = userService;
        this.jwtService = jwtService;
        this.enterValidator = enterValidator;
    }

    private final UserCredentialsEnterValidator enterValidator;

    @InitBinder("userCredentials")
    public void initBinder(WebDataBinder webDataBinder){
        webDataBinder.addValidators(enterValidator);
    }


    @PostMapping("/jwt")
    public String create(@RequestBody @Valid UserCredentials userCredentials, BindingResult bindingResult){
        if (bindingResult.hasErrors()){
            throw new ValidationException(bindingResult);
        }
        User user = userService.findByLoginAndPassword(userCredentials.getLogin(), userCredentials.getPassword());
        return jwtService.create(user);
    }

    @GetMapping("/jwt")
    public User find(@RequestParam String jwt){
        return jwtService.find(jwt);
    }

}
