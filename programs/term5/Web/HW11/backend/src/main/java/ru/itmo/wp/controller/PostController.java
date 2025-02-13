package ru.itmo.wp.controller;

import org.springframework.validation.BindingResult;
import org.springframework.validation.FieldError;
import org.springframework.web.bind.annotation.*;
import ru.itmo.wp.domain.Post;
import ru.itmo.wp.domain.User;
import ru.itmo.wp.form.PostForm;
import ru.itmo.wp.service.PostService;
import ru.itmo.wp.service.UserService;

import javax.validation.Valid;
import java.util.List;

@RestController
@RequestMapping("/api")
public class PostController {

    private final PostService postService;
    private final UserService userService;

    public PostController(PostService postService, UserService userService) {
        this.postService = postService;
        this.userService = userService;
    }

    @GetMapping("/posts")
    public List<Post> getAllPosts() {
        return postService.findAll();
    }

    @GetMapping("/posts/{id}")
    public Post getPost(@PathVariable String id) {
        try {
            return postService.find(Long.parseLong(id));
        } catch (NumberFormatException e) {
            return null;
        }
    }

    @PostMapping("/posts")
    public Post create(@RequestBody @Valid PostForm postForm) {
        User user = userService.findByLogin(postForm.getUserLogin());
        return postService.createPost(postForm, user);
    }
}
