package ru.itmo.wp.model.repository;

import ru.itmo.wp.model.domain.Talk;

import java.util.List;

public interface TalkRepository {
    Talk find(long id);

    List<Talk> findBySourceId(long sourceId);

    List<Talk> findByTargetId(long targetId);

    void save(Talk talk);

    List<Talk> findAllByUserId(long userId);
}
