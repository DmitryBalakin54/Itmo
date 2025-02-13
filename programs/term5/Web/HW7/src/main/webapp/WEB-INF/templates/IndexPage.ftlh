<#-- @ftlvariable name="articles" type="ru.itmo.wp.model.domain.Article[]" -->
<#import "commons.ftlh" as c>

<@c.page>
    <div class="articles">
        <template>
            <article class="some_article">
                <div class="title"></div>
                <div class="userLogin"></div>
                <div class="article_body">
                    <div class="text" style="white-space: pre-line;"></div>
                </div>
                <div class="creationTime"></div>
                <div class="footer">
                    <div class="left">
                        <img src="img/voteup.png" title="Vote Up" alt="Vote Up"/>
                        <span class="positive-score">+173</span>
                        <img src="img/votedown.png" title="Vote Down" alt="Vote Down"/>
                    </div>
                    <div class="right">
                        <img src="img/date_16x16.png" title="Publish Time" alt="Publish Time"/>
                        2 days ago
                        <img src="img/comments_16x16.png" title="Comments" alt="Comments"/>
                        <a href="#">68</a>
                    </div>
                </div>
            </article>
        </template>
        <div class="noData">
            No data
        </div>
    </div>

    <script>
        $(function () {
            const $noData = $(".articles .noData");
            $noData.text("Loading...");

            ajax({action: "findAll"}, function (response) {
                const articlesArray = new Array(response["articles"].length);
                const promises = [];

                response["articles"].forEach(function (article, index) {
                    const promise = new Promise(function(resolve) {
                        const $content = $($(".articles template").prop("content")).clone();
                        ajax({action: "findUser", userId: article["userId"]}, function(userResponse) {
                            $content.find(".userLogin").text(userResponse["userLogin"]);
                            $content.find(".title").text(article["title"]);
                            $content.find(".text").text(article["text"]);
                            $content.find(".creationTime").text(article["creationTime"]);

                            articlesArray[index] = $content;
                            resolve();
                        });
                    });

                    promises.push(promise);
                });

                Promise.all(promises).then(function() {
                    if (articlesArray.every(el => el === undefined)) {
                        $noData.text("No articles available");
                    } else {
                        $noData.hide();
                        articlesArray.forEach(function (el) {
                            $(".articles").append(el);
                        });
                    }
                });
            });
        });
    </script>
</@c.page>
