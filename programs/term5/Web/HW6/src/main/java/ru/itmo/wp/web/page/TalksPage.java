package ru.itmo.wp.web.page;

import ru.itmo.wp.model.domain.Talk;
import ru.itmo.wp.model.domain.User;
import ru.itmo.wp.web.exception.RedirectException;

import javax.servlet.http.HttpServletRequest;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class TalksPage extends Page {
    @Override
    protected void before(HttpServletRequest request, Map<String, Object> view) {
        super.before(request, view);

        if (getUser() == null) {
            setMessage("Talks only for registered users.");
            throw new RedirectException("/index");
        }

        putMessage(view);
    }

    @Override
    protected void action(HttpServletRequest request, Map<String, Object> view) {
        long id = getUser().getId();
        List<Talk> talks = talkService.findAllByUserId(id);

        Set<Long> userIds = talks.stream()
                .flatMap(talk -> Stream.of(talk.getSourceUserId(), talk.getTargetUserId()))
                .collect(Collectors.toSet());

        Map<Long, String> userLogins = userIds.stream()
                .collect(Collectors.toMap(userId -> userId, userId -> userService.findLoginById(userId)));

        talks.forEach(talk -> {
            talk.setSourceLogin(userLogins.get(talk.getSourceUserId()));
            talk.setTargetLogin(userLogins.get(talk.getTargetUserId()));
        });

        view.put("talks", talks);
        List<User> users = userService.findAll();
        User currentUser = getUser();
        List<User> filteredUsers = users.stream()
                .filter(user -> user.getId() != currentUser.getId())
                .collect(Collectors.toList());
        view.put("users", filteredUsers);
    }

    private void send(HttpServletRequest request, Map<String, Object> view) {
        long targetUserId = Long.parseLong(request.getParameter("targetLogin"));
        String text = request.getParameter("text");
        Talk talk = new Talk();
        talk.setText(text);
        talk.setTargetUserId(targetUserId);
        talk.setSourceUserId(getUser().getId());

        talkService.add(talk);
        throw new RedirectException("/talks");
    }
}
