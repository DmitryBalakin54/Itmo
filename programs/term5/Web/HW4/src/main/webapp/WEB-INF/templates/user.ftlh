<#-- @ftlvariable name="handle" type="java.lang.String" -->
<#-- @ftlvariable name="user_id" type="java.lang.Long" -->
<#import "./common.ftlh" as c>

<head>
    <link rel="stylesheet" href="/css/style.css">
</head>

<@c.page>
    <#if user_id?? && user_id?has_content>
        <#assign u = c.findBy(users, "id", user_id)!/>
    <#else>
        <#assign u = c.findBy(users, "handle", handle)!/>
    </#if>

    <#if u?has_content>
        <style>
            .user-info {
                border:var(--boder);
                border-radius: var(--border-radius);
                position: relative;
            }

            .user-info h1 {
                padding-left: 1rem;
            }

            .navigation {
                display: flex;
            }

            .user-image {
                position: absolute;
                right: 1rem;
                top: 1rem;
            }
        </style>

        <div class="user-info">
            <h1>
                <@c.userlink u/>
            </h1>

            <#assign user_params=[
            {"name": "id", "value": "${u.id}"},
            {"name": "handle", "value": "${u.handle}"},
            {"name": "name", "value": "${u.name}"}
            ]/>

            <ul>
                <#list user_params as param>
                    <li>
                        ${param.name}: ${param.value}
                    </li>
                </#list>
            </ul>

            <img src="/img/no-title.jpg" class="user-image"/>
        </div>



        <#assign currentIndex = c.getIndex(users, "handle", u.handle) />
        <#assign previousUser = users[currentIndex - 1]?if_exists/>
        <#assign nextUser = users[currentIndex + 1]?if_exists/>

        <div class="navigation">
            <@c.userlinkSign previousUser "←"/>
            <@c.userlinkSign nextUser " →"/>
        </div>

<#--        <@c.getAllPostsById true u.id/>-->
        <a href="/posts?user_id=${u.id}">View all posts by this user</a>
    <#else>
        No such user
    </#if>
</@c.page>
