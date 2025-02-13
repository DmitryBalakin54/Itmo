package ru.itmo.wp.controller;

import org.springframework.stereotype.Controller;
import org.springframework.ui.Model;
import org.springframework.validation.BindingResult;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.ModelAttribute;
import org.springframework.web.bind.annotation.PostMapping;
import ru.itmo.wp.domain.Post;
import ru.itmo.wp.domain.Role;
import ru.itmo.wp.domain.Tag;
import ru.itmo.wp.form.WritePostForm;
import ru.itmo.wp.security.AnyRole;
import ru.itmo.wp.service.TagService;
import ru.itmo.wp.service.UserService;

import javax.servlet.http.HttpSession;
import javax.validation.Valid;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

@Controller
public class WritePostPage extends Page {
    private final UserService userService;
    private final TagService tagService;

    public WritePostPage(UserService userService, TagService tagService) {
        this.userService = userService;
        this.tagService = tagService;
    }

    @AnyRole({Role.Name.WRITER, Role.Name.ADMIN})
    @GetMapping("/writePost")
    public String writePostGet(Model model) {
        model.addAttribute("postParams", new WritePostForm());
        return "WritePostPage";
    }

    @AnyRole({Role.Name.WRITER, Role.Name.ADMIN})
    @PostMapping("/writePost")
    public String writePostPost(@Valid @ModelAttribute("postParams") WritePostForm post,
                                BindingResult bindingResult,
                                HttpSession httpSession) {
        if (bindingResult.hasErrors()) {
          return "WritePostPage";
        }

        List<String> tagStrings = Arrays.stream(post.getTagsString().trim().split("\\s+"))
                .filter(s -> !s.isEmpty())
                .toList();

        Set<Tag> tagSet = new HashSet<>();
        for (String tagStr : tagStrings) {
            tagSet.add(tagService.findOrCreate(tagStr));
        }

        Post resPost = new Post();
        resPost.setTitle(post.getTitle());
        resPost.setText(post.getText());
        resPost.setTags(tagSet);
        userService.writePost(getUser(httpSession), resPost);
        putMessage(httpSession, "You published a new post!");

        return "redirect:/myPosts";
    }
}
