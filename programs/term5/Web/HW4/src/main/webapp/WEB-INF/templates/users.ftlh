<#import "./common.ftlh" as c>


<@c.page>
    <ul>
        <#list users as u>
            <li>
                <#if user?? && user.id == u.id>*</#if>
                <@c.userlink u/>
            </li>
        </#list>
    </ul>
</@c.page>
