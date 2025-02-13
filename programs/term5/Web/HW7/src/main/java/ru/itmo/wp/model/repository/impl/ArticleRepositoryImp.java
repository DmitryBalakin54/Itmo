package ru.itmo.wp.model.repository.impl;

import ru.itmo.wp.model.domain.Article;
import ru.itmo.wp.model.repository.AbstractRepository;
import ru.itmo.wp.model.repository.ArticleRepository;

import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.SQLException;
import java.util.List;
import java.util.Map;

public class ArticleRepositoryImp extends AbstractRepository<Article> implements ArticleRepository {
    @Override
    protected Article toEntity(ResultSetMetaData metaData, ResultSet resultSet) throws SQLException {
        if (!resultSet.next()) {
            return null;
        }

        Article article = new Article();
        for (int i = 1; i <= metaData.getColumnCount(); i++) {
            switch (metaData.getColumnName(i)) {
                case "id":
                    article.setId(resultSet.getLong(i));
                    break;
                case "userId":
                    article.setUserId(resultSet.getLong(i));
                    break;
                case "title":
                    article.setTitle(resultSet.getString(i));
                    break;
                case "text":
                    article.setText(resultSet.getString(i));
                    break;
                case "creationTime":
                    article.setCreationTime(resultSet.getTimestamp(i));
                    break;
                case "hidden":
                    article.setHidden(resultSet.getBoolean(i));
                    break;
                default:
                    // No operations.
            }
        }
        return article;
    }

    @Override
    protected String getTableName() {
        return "Article";
    }

    @Override
    protected String getSaveQuery() {
        return "INSERT INTO `Article` (`userId`, `title`, `text`, `hidden` , `creationTime`) VALUES (?, ?, ?, ?, NOW())";
    }

    @Override
    protected void setSaveStatementParams(PreparedStatement statement, Article article, Map<String, Object> params) throws SQLException {
        statement.setLong(1, article.getUserId());
        statement.setString(2, article.getTitle());
        statement.setString(3, article.getText());
        statement.setBoolean(4, article.getHidden());
    }

    @Override
    protected void setGeneratedIdAndTime(Article article, long generatedId) throws SQLException {
        article.setId(generatedId);
        article.setCreationTime(find(generatedId).getCreationTime());
    }

    @Override
    public List<Article> findAllByUserId(long userId) {
        return findAllByAny(Map.of("userId", userId), null);
    }

    @Override
    public void save(Article article) {
        save(article, Map.of());
    }

    @Override
    public List<Article> findAllOrdered() {
        return findAllByAny(Map.of(), "creationTime DESC");
    }

    @Override
    public List<Article> findAllOrderedChronology() {
        return findAllByAny(Map.of(), "creationTime ASC");
    }

    @Override
    public List<Article> findAllOrderedChronologyByUserId(long userId) {
        return findAllByAny(Map.of("userId", userId), "creationTime ASC");
    }

    @Override
    public List<Article> findAllOrderedNotHidden() {
        return findAllByAll(Map.of("hidden", Boolean.FALSE), "creationTime DESC");
    }


    @Override
    public Article toggleHiddenById(long id, boolean hidden) {
        try (PreparedStatement statement = DATA_SOURCE.getConnection().prepareStatement(
                "UPDATE `Article` SET `hidden`=? WHERE `id`=?")) {
            statement.setBoolean(1, hidden);
            statement.setLong(2, id);

            int rowsUpdated = statement.executeUpdate();

            if (rowsUpdated > 0) {
                return find(id);
            } else {
                return null;
            }
        } catch (SQLException e) {
            throw new RuntimeException("Unable to toggle hidden status for Article with id " + id, e);
        }
    }

}
