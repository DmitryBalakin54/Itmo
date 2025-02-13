package ru.itmo.wp.servlet;

import com.google.gson.Gson;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

public class MessageServlet extends HttpServlet {

    private List<Message> messages = new ArrayList<>();

    @Override
    protected void doPost(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
        String info = request.getPathInfo();
        switch (info) {
            case "/auth":
                auth(request, response);
                break;
            case "/findAll":
                findAll(response);
                break;
            case "/add":
                add(request, response);
                break;
            default:
                response.setStatus(HttpServletResponse.SC_NOT_FOUND);
                break;
        }
    }

    private void print(String data, HttpServletResponse response) throws IOException {
        response.setContentType("application/json");
        response.getWriter().print(data);
        response.setStatus(HttpServletResponse.SC_OK);
        response.getWriter().flush();
    }
    private void auth(HttpServletRequest request, HttpServletResponse response) throws IOException {
        String user = request.getParameter("user");
        if (user == null) {
            user = "";
        }

        request.getSession().setAttribute("user", user);
        print(new Gson().toJson(user), response);
    }

    private void findAll(HttpServletResponse response) throws IOException {
        print(new Gson().toJson(messages), response);
    }

    private void add(HttpServletRequest request, HttpServletResponse response) throws IOException {
        String user = request.getSession().getAttribute("user").toString();
        String text = request.getParameter("text");

        if (text != null) {
            messages.add(new Message(user, text));
        }

        print("{}", response);
    }

    class Message {
        private String user;
        private String text;

        public Message(String user, String text) {
            this.user = user;
            this.text = text;
        }
    }
}
