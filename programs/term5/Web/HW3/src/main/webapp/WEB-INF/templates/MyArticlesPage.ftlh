<#-- @ftlvariable name="articles" type="ru.itmo.wp.model.domain.Article[]" -->
<#import "commons.ftlh" as c>

<@c.page>
    <div class="article-list datatable">
        <div class="caption">My Articles</div>

        <table>
            <thead>
            <tr>
                <th>ID</th>
                <th>Title</th>
                <th>Visibility</th>
            </tr>
            </thead>
            <tbody>
            <#list articles as article>
                <tr>
                    <td>${article.id}</td>
                    <td>${article.title}</td>
                    <td>
                        <button class="toggle-hidden"
                                data-id="${article.id}"
                                data-hidden="${article.hidden?c}"
                                data-userId="${article.userId}">
                            <#if article.hidden>Show<#else>Hide</#if>
                        </button>
                    </td>
                </tr>
            </#list>
            </tbody>
        </table>
    </div>

    <script>
        $(function () {
            $(".toggle-hidden").click(function () {
                const $button = $(this);
                const id = $button.data("id");
                const isHidden = $button.data("hidden");
                const userId = $button.attr("data-userId")

                const data = {
                    action: "toggleHidden",
                    id: id,
                    hidden: !isHidden,
                    userId: userId
                };

                ajax(data, function (response) {
                    const newArticle = response["newArticle"];
                    $button.data("hidden", newArticle.hidden);
                    $button.text(!newArticle.hidden ? "Hide" : "Show");
                })
            });
        });
    </script>
</@c.page>
