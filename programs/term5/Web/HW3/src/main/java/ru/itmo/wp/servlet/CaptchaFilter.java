package ru.itmo.wp.servlet;

import javax.servlet.*;
import javax.servlet.http.*;
import java.io.IOException;
import java.util.Base64;
import java.util.Random;
import ru.itmo.wp.util.ImageUtils;

public class CaptchaFilter implements Filter {

    private final String CAPTCHA_PASSED = "captchaPassed";
    private final String CAPTCHA_EXPECTED = "captchaExpected";
    private final String CAPTCHA_ANSWER = "captchaAnswer";
    private final String WAY_TO_FILE = "wayToFile";

    @Override
    public void doFilter(ServletRequest request, ServletResponse response, FilterChain chain)
            throws IOException, ServletException {

        HttpServletRequest httpRequest = (HttpServletRequest) request;
        HttpServletResponse httpResponse = (HttpServletResponse) response;
        HttpSession session = httpRequest.getSession();

        Boolean captchaPassed = (Boolean) session.getAttribute(CAPTCHA_PASSED);
        if (captchaPassed != null && captchaPassed) {
            chain.doFilter(request, response);
            return;
        }

        if (httpRequest.getMethod().equalsIgnoreCase("POST")) {
            String ans = session.getAttribute(CAPTCHA_EXPECTED).toString();
            String userAnswer = httpRequest.getParameter(CAPTCHA_ANSWER);

            if (userAnswer != null && userAnswer.equals(ans)) {
                session.setAttribute(CAPTCHA_PASSED, true);
                httpResponse.sendRedirect(session.getAttribute(WAY_TO_FILE).toString());
                //chain.doFilter(request, response);
            } else {
                generateCaptcha(httpRequest, httpResponse);
            }

            return;
        }

        if (httpRequest.getMethod().equalsIgnoreCase("GET") && !httpRequest.getRequestURI().endsWith("/favicon.ico")) {
            session.setAttribute(WAY_TO_FILE, httpRequest.getRequestURI());
            generateCaptcha(httpRequest, httpResponse);
        }
    }

    private void generateCaptcha(HttpServletRequest request, HttpServletResponse response) throws IOException {
        HttpSession session = request.getSession();
        int captcha = 100 + new Random().nextInt(900);

        session.setAttribute(CAPTCHA_EXPECTED, String.valueOf(captcha));
        String img = Base64.getEncoder().encodeToString(ImageUtils.toPng(String.valueOf(captcha)));

        response.setContentType("text/html");
        response.getWriter().write(
        "<html>\n" +
                "<body>\n" +
                "<h1>Captcha &#129402</h1>\n" +
                "<img src='data:image/png;base64," + img + "' /><br>\n" +
                "<form method='POST'>\n" +
                    "Enter the number: <input type='text' name='" + CAPTCHA_ANSWER + "' />\n" +
                    "<input type='submit' value='Submit' />\n" +
                "</form>\n" +
            "</body>\n" +
        "</html>"
        );

        response.getWriter().flush();
    }
}
