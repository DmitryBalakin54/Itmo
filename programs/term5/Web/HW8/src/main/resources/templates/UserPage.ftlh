<#import "commons.ftlh" as c>

<@c.page "/user">
    <#if user_data??>
        <div>id: ${user_data.id}</div>
        <div>login: ${user_data.login}</div>
        <div>creationTime: ${user_data.creationTime}</div>
    <#else>
        No such user.
    </#if>
</@c.page>
