package ru.itmo.wp.model.service;

import ru.itmo.wp.model.domain.Talk;
import ru.itmo.wp.model.repository.TalkRepository;
import ru.itmo.wp.model.repository.impl.TalkRepositoryImpl;

import java.util.List;

public class TalkService {
    private final TalkRepository talkRepository = new TalkRepositoryImpl();

    public List<Talk> findBySourceId(long sourceId) {
       return talkRepository.findBySourceId(sourceId);
    }

    public List<Talk> findByTargetId(long targetId) {
        return talkRepository.findByTargetId(targetId);
    }

    public List<Talk> findBySoursAndTargetUserIds(long sourceId, long targetId) {
        List<Talk> bySource = findBySourceId(sourceId);
        List<Talk> byTarget = findByTargetId(targetId);

        bySource.addAll(byTarget);
        return bySource;
    }

    public void add(Talk talk) {
        talkRepository.save(talk);
    }

    public List<Talk> findAllByUserId(long userId) {
        return talkRepository.findAllByUserId(userId);
    }
}
