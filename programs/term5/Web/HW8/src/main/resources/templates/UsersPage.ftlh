<#import "commons.ftlh" as c>
<#import "/spring.ftl" as spring>

<@c.page "/users">
    <div class="users datatable">
        <div class="caption">Users</div>
        <table>
            <thead>
            <tr>
                <th>Id</th>
                <th>Login</th>
                <th>Created</th>
                <th>Status</th>
            </tr>
            </thead>
            <tbody>
            <#if users?size == 0>
                <tr class="noData">
                    <td colspan="4">No data</td>
                </tr>
            <#else>
                <#list users as user>
                    <tr>
                        <td class="id">${user.id}</td>
                        <td class="login">
                            <a href="<@spring.url '/user/${user.id}'/>">${user.login}</a>
                        </td>
                        <td class="creationTime">${user.creationTime}</td>
                        <td class="status">
                            <form method="post" action="">
                                <input type="hidden" name="id" value="${user.id}"/>
                                <input type="hidden" name="disabled" value="<#if user.disabled> false <#else> true </#if>"/>
                                <button type="submit">
                                    <#if user.disabled>
                                        Enable
                                    <#else>
                                        Disable
                                    </#if>
                                </button>
                            </form>
                        </td>
                    </tr>
                </#list>
            </#if>
            </tbody>
        </table>
    </div>
</@c.page>
