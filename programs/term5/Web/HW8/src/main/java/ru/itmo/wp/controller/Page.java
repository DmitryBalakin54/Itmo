package ru.itmo.wp.controller;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.ui.Model;
import org.springframework.validation.BindingResult;
import org.springframework.web.bind.WebDataBinder;
import org.springframework.web.bind.annotation.InitBinder;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.PostMapping;
import ru.itmo.wp.domain.Notice;
import ru.itmo.wp.domain.User;
import ru.itmo.wp.form.NoticeText;
import ru.itmo.wp.form.validator.NoticeFormValidator;
import ru.itmo.wp.service.NoticeService;
import ru.itmo.wp.service.UserService;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpSession;
import javax.validation.Valid;
import java.util.Enumeration;
import java.util.List;
import java.util.Map;

public abstract class Page {
    private static final String USER_ID_SESSION_KEY = "userId";
    private static final String MESSAGE_SESSION_KEY = "message";
    private static final String MESSAGE_SESSION_KEY_ERROR = "messageError";

    @InitBinder
    public void initBinderPage(WebDataBinder binder) {
        if (binder.getTarget() instanceof NoticeText) {
            binder.addValidators(new NoticeFormValidator());
        }
    }

    protected abstract String getPage();

    @Autowired
    private UserService userService;

    @Autowired
    private NoticeService noticeService;

    protected void getMethodAction(Model model) {
        model.addAttribute("noticeForm", new NoticeText());
    }

    @ModelAttribute("user")
    public User getUser(HttpSession httpSession) {
        return userService.findById((Long) httpSession.getAttribute(USER_ID_SESSION_KEY));
    }

    @ModelAttribute("notices")
    public List<Notice> getNotices() {
        return noticeService.findAll();
    }

    @ModelAttribute("message")
    public String getMessage(HttpSession httpSession) {
        String message = (String) httpSession.getAttribute(MESSAGE_SESSION_KEY);
        httpSession.removeAttribute(MESSAGE_SESSION_KEY);
        return message;
    }

    @ModelAttribute("messageError")
    public String getMessageError(HttpSession httpSession) {
        String message = (String) httpSession.getAttribute(MESSAGE_SESSION_KEY_ERROR);
        httpSession.removeAttribute(MESSAGE_SESSION_KEY_ERROR);
        return message;
    }

    protected void setUser(HttpSession httpSession, User user) {
        if (user != null) {
            httpSession.setAttribute(USER_ID_SESSION_KEY, user.getId());
        } else {
            unsetUser(httpSession);
        }
    }

    protected void unsetUser(HttpSession httpSession) {
        httpSession.removeAttribute(USER_ID_SESSION_KEY);
    }

    protected void setMessage(HttpSession httpSession, String message) {
        httpSession.setAttribute(MESSAGE_SESSION_KEY, message);
    }

    protected void setMessageError(HttpSession httpSession, String message) {
        httpSession.setAttribute(MESSAGE_SESSION_KEY_ERROR, message);
    }

    @PostMapping("/addNotice")
    public String addNoticePost(@Valid @ModelAttribute("noticeForm") NoticeText noticeText,
                                BindingResult bindingResult,
                                HttpSession httpSession,
                                HttpServletRequest request,
                                Model model) {
        if (bindingResult.hasErrors()) {
            setMessageError(httpSession,"Notice error: " + bindingResult.getAllErrors().get(0).getDefaultMessage());
            return "redirect:" + request.getHeader("Referer");
        }

        if (noticeService.addNotice(noticeText) == null) {
            setMessageError(httpSession,"Notice must be less 256 characters");
            return "redirect:" + request.getHeader("Referer");

        }
        setMessage(httpSession, "Notice sent");

        return "redirect:" + request.getHeader("Referer");
    }
}
