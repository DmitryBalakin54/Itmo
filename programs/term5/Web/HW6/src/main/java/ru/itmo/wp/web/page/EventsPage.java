package ru.itmo.wp.web.page;

import javax.servlet.http.HttpServletRequest;
import java.util.Map;

public class EventsPage extends Page {
    @Override
    protected void action(HttpServletRequest request, Map<String, Object> view) {
        view.put("events", eventService.findAll());
    }
}
