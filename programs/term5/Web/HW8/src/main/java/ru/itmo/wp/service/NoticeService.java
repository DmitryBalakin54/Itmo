package ru.itmo.wp.service;

import org.springframework.stereotype.Service;
import ru.itmo.wp.domain.Notice;
import ru.itmo.wp.form.NoticeText;
import ru.itmo.wp.repository.NoticeRepository;

import java.util.List;

@Service
public class NoticeService {
    private final NoticeRepository noticeRepository;

    public NoticeService(NoticeRepository noticeRepository) {
        this.noticeRepository = noticeRepository;
    }

    public List<Notice> findAll() {
        return noticeRepository.findAll();
    }

    public Notice addNotice(NoticeText noticeText) {
        Notice notice = new Notice();
        notice.setContent(noticeText.getContent());
        if (!isValid(notice)) {
            return null;
        }
        noticeRepository.save(notice);
        return notice;
    }

    public boolean isValid(Notice notice) {
        return notice.getContent().length() < 256;
    }
}
