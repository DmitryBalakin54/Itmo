package ru.itmo.wp.controller;

import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.validation.BindingResult;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import ru.itmo.wp.domain.Comment;
import ru.itmo.wp.domain.Post;
import ru.itmo.wp.security.Guest;
import ru.itmo.wp.service.CommentService;
import ru.itmo.wp.service.PostService;
import ru.itmo.wp.service.UserService;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpSession;
import javax.validation.Valid;

@Controller
public class PostPage extends Page {
    private final PostService postService;
    private final CommentService commentService;
    private final UserService userService;

    public PostPage(PostService postService, CommentService commentService, UserService userService) {
        this.postService = postService;
        this.commentService = commentService;
        this.userService = userService;
    }

    @Guest
    @GetMapping({"/post/{id}"})
    public String post(Model model, @PathVariable String id) {
        Post post = null;
        try {
            post = postService.findById(Long.parseLong(id));
        } catch (NumberFormatException ignored) {}

        model.addAttribute("post", post);
        model.addAttribute("comments", commentService.getCommentsForPost(post));
        model.addAttribute("comment", new Comment());
        return "PostPage";
    }

    @Guest
    @GetMapping({"/post/", "/post"})
    public String emptyPost(Model model) {
        System.out.println("EMPTY POST");
        model.addAttribute("post", null);
        return "PostPage";
    }


    @PostMapping("/post/{id}")
    public String writeCommentPost(@PathVariable String id,
                                   @Valid @ModelAttribute("comment") Comment comment,
                                   BindingResult bindingResult,
                                   HttpSession httpSession,
                                   HttpServletRequest request) {
        if (bindingResult.hasErrors()) {
            return "redirect:" + request.getHeader("Referer");
        }

        Post post = postService.findById(Long.parseLong(id));
        postService.writeComment(getUser(httpSession), post, comment);
        putMessage(httpSession, "You wrote new comment");

        return "redirect:/post/" + post.getId();
    }
}
