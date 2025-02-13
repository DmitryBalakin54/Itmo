package ru.itmo.wp.form.validator;

import org.springframework.stereotype.Component;
import org.springframework.validation.Errors;
import org.springframework.validation.Validator;
import ru.itmo.wp.form.NoticeText;

@Component
public class NoticeFormValidator implements Validator {
    public boolean supports(Class<?> clazz) {
        return clazz.equals(NoticeText.class);
    }

    @Override
    public void validate(Object target, Errors errors) {
        if (errors.hasErrors()) {
            return;
        }

        NoticeText noticeText = (NoticeText) target;
        if (noticeText.getContent().trim().isEmpty()) {
            errors.rejectValue("content", "notice.content.invalid", "is empty");
        }
    }
}
