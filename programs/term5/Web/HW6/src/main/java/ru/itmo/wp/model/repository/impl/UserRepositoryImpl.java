package ru.itmo.wp.model.repository.impl;

import ru.itmo.wp.model.domain.User;
import ru.itmo.wp.model.exception.RepositoryException;
import ru.itmo.wp.model.repository.AbstractRepository;
import ru.itmo.wp.model.repository.UserRepository;
import ru.itmo.wp.model.service.UserService;

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
                case "email":
                    user.setEmail(resultSet.getString(i));
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
        return "INSERT INTO `User` (`login`, `passwordSha`, `email`, `creationTime`) VALUES (?, ?, ?, NOW())";
    }

    @Override
    protected void setSaveStatementParams(PreparedStatement statement, User user, Map<String, Object> params) throws SQLException {
        statement.setString(1, user.getLogin());
        statement.setString(2, params.get("passwordSha").toString());
        statement.setString(3, user.getEmail());
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
    public User findByEmail(String email) {
        return findBy(Map.of("email", email));
    }

    @Override
    public User findByLoginAndPasswordSha(String login, String passwordSha) {
        return findBy(Map.of("login", login, "passwordSha", passwordSha));
    }

    @Override
    public User findByEmailAndPasswordSha(String email, String passwordSha) {
        return findBy(Map.of("email", email, "passwordSha", passwordSha));
    }

    @Override
    public User findByLoginOrEmailAndPasswordSha(String loginOrEmail, String passwordSha) {
        if (UserService.isEmailValid(loginOrEmail)) {
            return findByEmailAndPasswordSha(loginOrEmail, passwordSha);
        }

        return findByLoginAndPasswordSha(loginOrEmail, passwordSha);
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


//    private final DataSource DATA_SOURCE = DatabaseUtils.getDataSource();
//
//    @Override
//    public User find(long id) {
//        try (Connection connection = DATA_SOURCE.getConnection()) {
//            try (PreparedStatement statement = connection.prepareStatement("SELECT * FROM User WHERE id=?")) {
//                statement.setLong(1, id);
//                try (ResultSet resultSet = statement.executeQuery()) {
//                    return toUser(statement.getMetaData(), resultSet);
//                }
//            }
//        } catch (SQLException e) {
//            throw new RepositoryException("Can't find User.", e);
//        }
//    }
//
//    @Override
//    public User findByLogin(String login) {
//        try (Connection connection = DATA_SOURCE.getConnection()) {
//            try (PreparedStatement statement = connection.prepareStatement("SELECT * FROM User WHERE login=?")) {
//                statement.setString(1, login);
//                try (ResultSet resultSet = statement.executeQuery()) {
//                    return toUser(statement.getMetaData(), resultSet);
//                }
//            }
//        } catch (SQLException e) {
//            throw new RepositoryException("Can't find User.", e);
//        }
//    }
//
//    @Override
//    public User findByEmail(String email) {
//        try (Connection connection = DATA_SOURCE.getConnection()) {
//            try (PreparedStatement statement = connection.prepareStatement("SELECT * FROM User WHERE email=?")) {
//                statement.setString(1, email);
//                try (ResultSet resultSet = statement.executeQuery()) {
//                    return toUser(statement.getMetaData(), resultSet);
//                }
//            }
//        } catch (SQLException e) {
//            throw new RepositoryException("Can't find User.", e);
//        }
//    }
//
//    @Override
//    public User findByLoginAndPasswordSha(String login, String passwordSha) {
//        try (Connection connection = DATA_SOURCE.getConnection()) {
//            try (PreparedStatement statement = connection.prepareStatement("SELECT * FROM User WHERE login=? AND passwordSha=?")) {
//                statement.setString(1, login);
//                statement.setString(2, passwordSha);
//                try (ResultSet resultSet = statement.executeQuery()) {
//                    return toUser(statement.getMetaData(), resultSet);
//                }
//            }
//        } catch (SQLException e) {
//            throw new RepositoryException("Can't find User.", e);
//        }
//    }
//
//    @Override
//    public User findByEmailAndPasswordSha(String email, String passwordSha) {
//        try (Connection connection = DATA_SOURCE.getConnection()) {
//            try (PreparedStatement statement = connection.prepareStatement("SELECT * FROM User WHERE email=? AND passwordSha=?")) {
//                statement.setString(1, email);
//                statement.setString(2, passwordSha);
//                try (ResultSet resultSet = statement.executeQuery()) {
//                    return toUser(statement.getMetaData(), resultSet);
//                }
//            }
//        } catch (SQLException e) {
//            throw new RepositoryException("Can't find User.", e);
//        }
//    }
//
//    @Override
//    public User findByLoginOrEmailAndPasswordSha(String loginOrEmail, String passwordSha) {
//        if (UserService.isEmailValid(loginOrEmail)) {
//            return findByEmailAndPasswordSha(loginOrEmail, passwordSha);
//        }
//
//        return findByLoginAndPasswordSha(loginOrEmail, passwordSha);
//    }
//
//
//    @Override
//    public List<User> findAll() {
//        List<User> users = new ArrayList<>();
//        try (Connection connection = DATA_SOURCE.getConnection()) {
//            try (PreparedStatement statement = connection.prepareStatement("SELECT * FROM User ORDER BY id DESC")) {
//                try (ResultSet resultSet = statement.executeQuery()) {
//                    User user;
//                    while ((user = toUser(statement.getMetaData(), resultSet)) != null) {
//                        users.add(user);
//                    }
//                }
//            }
//        } catch (SQLException e) {
//            throw new RepositoryException("Can't find User.", e);
//        }
//        return users;
//    }
//
//    private User toUser(ResultSetMetaData metaData, ResultSet resultSet) throws SQLException {
//        if (!resultSet.next()) {
//            return null;
//        }
//
//        User user = new User();
//        for (int i = 1; i <= metaData.getColumnCount(); i++) {
//            switch (metaData.getColumnName(i)) {
//                case "id":
//                    user.setId(resultSet.getLong(i));
//                    break;
//                case "login":
//                    user.setLogin(resultSet.getString(i));
//                    break;
//                case "creationTime":
//                    user.setCreationTime(resultSet.getTimestamp(i));
//                    break;
//                case "email":
//                    user.setEmail(resultSet.getString(i));
//                    break;
//                default:
//                    // No operations.
//            }
//        }
//
//        return user;
//    }
//
//    @Override
//    public void save(User user, String passwordSha) {
//        try (Connection connection = DATA_SOURCE.getConnection()) {
//            try (PreparedStatement statement = connection.prepareStatement(
//                    "INSERT INTO `User` (`login`, `passwordSha`, `email` , `creationTime`) VALUES (?, ?, ?, NOW())",
//                    Statement.RETURN_GENERATED_KEYS
//            )) {
//                statement.setString(1, user.getLogin());
//                statement.setString(2, passwordSha);
//                statement.setString(3, user.getEmail());
//                if (statement.executeUpdate() != 1) {
//                    throw new RepositoryException("Can't save User.");
//                } else {
//                    ResultSet generatedKeys = statement.getGeneratedKeys();
//                    if (generatedKeys.next()) {
//                        user.setId(generatedKeys.getLong(1));
//                        user.setCreationTime(find(user.getId()).getCreationTime());
//                    } else {
//                        throw new RepositoryException("Can't save User [no autogenerated fields].");
//                    }
//                }
//            }
//        } catch (SQLException e) {
//            throw new RepositoryException("Can't save User.", e);
//        }
//    }
//
//    @Override
//    public long findCount() {
//        try (Connection connection = DATA_SOURCE.getConnection()) {
//            try (PreparedStatement statement = connection.prepareStatement("SELECT COUNT(*) FROM User")) {
//                try (ResultSet resultSet = statement.executeQuery()) {
//                    if (resultSet.next()) {
//                        return resultSet.getLong(1);
//                    }
//                }
//            }
//        } catch (SQLException e) {
//            throw new RepositoryException("Can't find User count.", e);
//        }
//
//        return 0;
//    }
}
