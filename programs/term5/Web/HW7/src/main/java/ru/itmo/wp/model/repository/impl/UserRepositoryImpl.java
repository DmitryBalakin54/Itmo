package ru.itmo.wp.model.repository.impl;

import ru.itmo.wp.model.domain.Article;
import ru.itmo.wp.model.domain.User;
import ru.itmo.wp.model.exception.RepositoryException;
import ru.itmo.wp.model.repository.AbstractRepository;
import ru.itmo.wp.model.repository.UserRepository;

import java.sql.*;
import java.util.Map;

@SuppressWarnings("SqlNoDataSourceInspection")
public class UserRepositoryImpl extends AbstractRepository<User> implements UserRepository {

    @Override
    protected User toEntity(ResultSetMetaData metaData, ResultSet resultSet) throws SQLException {
        if (!resultSet.next()) {
            return null;
        }

        User user = new User();
        for (int i = 1; i <= metaData.getColumnCount(); i++) {
            switch (metaData.getColumnName(i)) {
                case "id":
                    user.setId(resultSet.getLong(i));
                    break;
                case "login":
                    user.setLogin(resultSet.getString(i));
                    break;
                case "creationTime":
                    user.setCreationTime(resultSet.getTimestamp(i));
                    break;
                case "admin":
                    user.setAdmin(resultSet.getBoolean(i));
                    break;
                default:
                    // No operations.
            }
        }
        return user;
    }

    @Override
    protected String getTableName() {
        return "User";
    }

    @Override
    protected String getSaveQuery() {
        return "INSERT INTO `User` (`login`, `passwordSha`, `creationTime`) VALUES (?, ?, NOW())";
    }

    @Override
    protected void setSaveStatementParams(PreparedStatement statement, User user, Map<String, Object> params) throws SQLException {
        statement.setString(1, user.getLogin());
        statement.setString(2, params.get("passwordSha").toString());
    }

    @Override
    protected void setGeneratedIdAndTime(User user, long generatedId) throws SQLException {
        user.setId(generatedId);
        user.setCreationTime(find(generatedId).getCreationTime());
    }

    @Override
    public User findByLogin(String login) {
        return findBy(Map.of("login", login));
    }

    @Override
    public User findByLoginAndPasswordSha(String login, String passwordSha) {
        return findBy(Map.of("login", login, "passwordSha", passwordSha));
    }

    @Override
    public void save(User user, String passwordSha) {
        save(user, Map.of("passwordSha", passwordSha));
    }

    @Override
    public long findCount() {
        try (Connection connection = DATA_SOURCE.getConnection()) {
            try (PreparedStatement statement = connection.prepareStatement("SELECT COUNT(*) FROM User")) {
                try (ResultSet resultSet = statement.executeQuery()) {
                    if (resultSet.next()) {
                        return resultSet.getLong(1);
                    }
                }
            }
        } catch (SQLException e) {
            throw new RepositoryException("Can't find User count.", e);
        }

        return 0;
    }

    @Override
    public User toggleAdminStatusById(long id, boolean admin) {
        try (PreparedStatement statement = DATA_SOURCE.getConnection().prepareStatement(
                "UPDATE `User` SET `admin`=? WHERE `id`=?")) {
            statement.setBoolean(1, admin);
            statement.setLong(2, id);

            int rowsUpdated = statement.executeUpdate();

            if (rowsUpdated > 0) {
                return find(id);
            } else {
                return null;
            }
        } catch (SQLException e) {
            throw new RuntimeException("Unable to toggle admin status for User with id " + id, e);
        }
    }

}