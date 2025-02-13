package ru.itmo.wp.model.repository.impl;

import ru.itmo.wp.model.domain.Talk;
import ru.itmo.wp.model.repository.AbstractRepository;
import ru.itmo.wp.model.repository.TalkRepository;

import java.sql.*;
import java.util.List;
import java.util.Map;

public class TalkRepositoryImpl extends AbstractRepository<Talk> implements TalkRepository {
    @Override
    protected Talk toEntity(ResultSetMetaData metaData, ResultSet resultSet) throws SQLException {
        if (!resultSet.next()) {
            return null;
        }

        Talk talk = new Talk();
        for (int i = 1; i <= metaData.getColumnCount(); i++) {
            switch (metaData.getColumnName(i)) {
                case "id":
                    talk.setId(resultSet.getLong(i));
                    break;
                case "sourceUserId":
                    talk.setSourceUserId(resultSet.getLong(i));
                    break;
                case "targetUserId":
                    talk.setTargetUserId(resultSet.getLong(i));
                    break;
                case "text":
                    talk.setText(resultSet.getString(i));
                    break;
                case "creationTime":
                    talk.setCreationTime(resultSet.getTimestamp(i));
                default:
                    // No operations.
            }
        }

        return talk;
    }

    @Override
    protected String getTableName() {
        return "Talk";
    }

    @Override
    protected String getSaveQuery() {
        return "INSERT INTO `Talk` (`sourceUserId`, `targetUserId`, `text` , `creationTime`) VALUES (?, ?, ?, NOW())";
    }

    @Override
    protected void setSaveStatementParams(PreparedStatement statement, Talk talk, Map<String, Object> params) throws SQLException {
        statement.setLong(1, talk.getSourceUserId());
        statement.setLong(2, talk.getTargetUserId());
        statement.setString(3, talk.getText());
    }

    @Override
    protected void setGeneratedIdAndTime(Talk talk, long generatedId) throws SQLException {
        talk.setId(generatedId);
        talk.setCreationTime(find(talk.getId()).getCreationTime());
    }

    @Override
    public List<Talk> findBySourceId(long sourceId) {
        return findAllByAny(Map.of("sourceUserId", sourceId));
    }

    @Override
    public List<Talk> findByTargetId(long targetId) {
        return findAllByAny(Map.of("targetUserId", targetId));
    }

    @Override
    public void save(Talk talk) {
        save(talk, Map.of());
    }

    @Override
    public List<Talk> findAllByUserId(long userId) {
        return findAllByAny(Map.of("sourceUserId", userId, "targetUserId", userId));
    }


//    private final DataSource DATA_SOURCE = DatabaseUtils.getDataSource();
//
//    @Override
//    public Talk find(long id) {
//        try (Connection connection = DATA_SOURCE.getConnection()) {
//            try (PreparedStatement statement = connection.prepareStatement("SELECT * FROM Talk WHERE id=?")) {
//                statement.setLong(1, id);
//                try (ResultSet resultSet = statement.executeQuery()) {
//                    return toTalk(statement.getMetaData(), resultSet);
//                }
//            }
//        } catch (SQLException e) {
//            throw new RepositoryException("Can't find Talk.", e);
//        }
//    }
//
//    @Override
//    public List<Talk> findBySourceId(long sourceId) {
//        List<Talk> talks = new ArrayList<>();
//        try (Connection connection = DATA_SOURCE.getConnection()) {
//            try (PreparedStatement statement = connection.prepareStatement("SELECT * FROM Talk WHERE sourceUserId=?")) {
//                getTalks(sourceId, talks, statement);
//            }
//        } catch (SQLException e) {
//            throw new RepositoryException("Can't find Talks.", e);
//        }
//
//        return talks;
//    }
//
//    private void getTalks(long sourceId, List<Talk> talks, PreparedStatement statement) throws SQLException {
//        statement.setLong(1, sourceId);
//        try (ResultSet resultSet = statement.executeQuery()) {
//            Talk talk;
//            do {
//                talk = toTalk(statement.getMetaData(), resultSet);
//                if (talk != null) {
//                    talks.add(talk);
//                }
//
//            } while (talk != null);
//        }
//    }
//
//    @Override
//    public List<Talk> findByTargetId(long targetId) {
//        List<Talk> talks = new ArrayList<>();
//        try (Connection connection = DATA_SOURCE.getConnection()) {
//            try (PreparedStatement statement = connection.prepareStatement("SELECT * FROM Talk WHERE targetUserId=?")) {
//                getTalks(targetId, talks, statement);
//            }
//        } catch (SQLException e) {
//            throw new RepositoryException("Can't find Talks.", e);
//        }
//
//        return talks;
//    }
//
//    @Override
//    public void save(Talk talk) {
//        try (Connection connection = DATA_SOURCE.getConnection()) {
//            try (PreparedStatement statement = connection.prepareStatement(
//                    "INSERT INTO `Talk` (`sourceUserId`, `targetUserId`, `text` , `creationTime`) VALUES (?, ?, ?, NOW())",
//                    Statement.RETURN_GENERATED_KEYS
//            )) {
//                statement.setLong(1, talk.getSourceUserId());
//                statement.setLong(2, talk.getTargetUserId());
//                statement.setString(3, talk.getText());
//                if (statement.executeUpdate() != 1) {
//                    throw new RepositoryException("Can't save Talk.");
//                } else {
//                    ResultSet generatedKeys = statement.getGeneratedKeys();
//                    if (generatedKeys.next()) {
//                        talk.setId(generatedKeys.getLong(1));
//                        talk.setCreationTime(find(talk.getId()).getCreationTime());
//                    } else {
//                        throw new RepositoryException("Can't save Talk [no autogenerated fields].");
//                    }
//                }
//            }
//        } catch (SQLException e) {
//            throw new RepositoryException("Can't save Talk.", e);
//        }
//    }
//
//    private Talk toTalk(ResultSetMetaData metaData, ResultSet resultSet) throws SQLException {
//        if (!resultSet.next()) {
//            return null;
//        }
//
//        Talk talk = new Talk();
//        for (int i = 1; i <= metaData.getColumnCount(); i++) {
//            switch (metaData.getColumnName(i)) {
//                case "id":
//                    talk.setId(resultSet.getLong(i));
//                    break;
//                case "sourceUserId":
//                    talk.setSourceUserId(resultSet.getLong(i));
//                    break;
//                case "targetUserId":
//                    talk.setTargetUserId(resultSet.getLong(i));
//                    break;
//                case "text":
//                    talk.setText(resultSet.getString(i));
//                    break;
//                case "creationTime":
//                    talk.setCreationTime(resultSet.getTimestamp(i));
//                default:
//                    // No operations.
//            }
//        }
//
//        return talk;
//    }
//
//    public List<Talk> findAllByUserId(long userId) {
//        List<Talk> talks = new ArrayList<>();
//        try (Connection connection = DATA_SOURCE.getConnection()) {
//            try (PreparedStatement statement = connection.prepareStatement(
//                    "SELECT * FROM Talk WHERE sourceUserId = ? OR targetUserId = ? ORDER BY creationTime"
//            )) {
//                statement.setLong(1, userId);
//                statement.setLong(2, userId);
//                try (ResultSet resultSet = statement.executeQuery()) {
//                    getTalks(userId, talks, statement);
//                }
//            }
//        } catch (SQLException e) {
//            throw new RepositoryException("Can't find Talks.", e);
//        }
//        return talks;
//    }
}
