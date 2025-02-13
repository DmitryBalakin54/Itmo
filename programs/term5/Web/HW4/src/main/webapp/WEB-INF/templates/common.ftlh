<#-- @ftlvariable name="current_page" type="java.lang.String" -->
<#macro page>
    <!DOCTYPE html>
    <html lang="ru">
    <head>
        <meta charset="UTF-8">
        <title>Codeforces</title>
        <link rel="stylesheet" href="/css/normalize.css">
        <link rel="stylesheet" href="/css/style.css">
        <link rel="icon" href="/favicon.ico">
    </head>
    <body>
    <@header/>
    <div class="middle">
        <@sidebar 3/>
        <main>
            <#nested/>
        </main>
    </div>
    <@footer/>
    </body>
    </html>
</#macro>

<#macro header>
    <style>
        .active {
            text-decoration: underline;
            text-decoration-thickness: 0.2rem;
        }

         main article .title {
             color: var(--caption-color);
             font-size: 2rem;
             font-weight: bold;
         }

        main article .information {
            color: var(--border-color);
            font-size: 1rem;
        }

        main article .body {
            border-left: solid;
            padding-left: 1rem;
            border-width: 4px;
            border-color: var(--border-color);
            font-size: 1rem;
        }

        main article .attachment {
            padding-left: 2rem;
            font-size: 1rem;
        }

        main article .attachment li {
            color: var(--border-color);
            list-style-type: none;
            padding-left: 20px;
            padding-bottom: 4px;
            margin-left: -22px;;
        }

        main .footer {
            border: 1px solid var(--border-color);
            position: relative;
        }

        main .footer {
            padding: 5px;
            border-radius: var(--border-radius);
        }

        main .footer img {
            vertical-align: -2px;
            margin-right: -3px;
        }

        main .left span {
            margin: 0 5px;
            font-weight: bold;
        }

        main .positive-score {
            color: green;
        }

        main .left {
            display: flex;
        }

        main .right {
            position: absolute;
            right: 1rem;
            top: 7px;
        }

    </style>


    <header>
        <a href="#" class="logo"><img alt="Codeforces" title="Codeforces"
                                      src="/img/codeforces-logo-with-telegram.png"></a>
        <div class="languages">
            <a href="#"><img alt="In English" title="In English"
                             src="/img/gb.png"></a>
            <a href="#"><img alt="In Russian" title="In Russian"
                             src="/img/ru.png"></a>
        </div>
        <div class="enter-or-register-box">
            <#if user??>
                <@userlink user true/>
                |
                <a href="/logout">Logout</a>
            <#else>
                <a href="/enter">Enter</a>
                |
                <a href="/register">Register</a>
            </#if>
        </div>
        <nav>
            <#assign items=[
            {"href": "/index", "name": "Home"},
            {"href": "/misc/help", "name": "Help"},
            {"href": "/users", "name": "Users"},
            {"href": "/contests", "name": "Contests"}
            ]/>
            <ul>
                <#list items as item>
                    <li>
                        <a <#if item.href == current_page> class="active"</#if> href="${item.href}">${item.name}</a>
                    </li>
                </#list>
            </ul>
        </nav>
    </header>
</#macro>

<#macro oneOfSideBar post>
    <section>
        <div class="header">Information</div>
        <div class="body">
            <@onePostText post false/>
        </div>
        <div class="footer">
            <a href="/post?post_id=${post.id}">View all</a>
        </div>
    </section>
</#macro>

<#macro sidebar cnt>
    <aside>
        <#assign i = 0/>
        <#list sideBarPosts as post>
            <@oneOfSideBar post/>
            <#assign i = i + 1/>
            <#if (i == cnt)>
                <#break/>
            </#if>
        </#list>
    </aside>
</#macro>

<#macro footer>
    <footer>
        2010-2021 &copy; by Mike Mirzayanov
    </footer>
</#macro>

<#macro userlink user nameOnly=false>
    <a href="/user?handle=${user.handle}" style="text-decoration: none; <#if !nameOnly>font-weight: bold; color: <@getUserColor user.color/>;</#if>">
        ${user.name}
    </a>
</#macro>

<#macro getUserColor color>
    <#switch color>
        <#case "RED">
            red
            <#break>
        <#case "GREEN">
            green
            <#break>
        <#case "BLUE">
            blue
            <#break>
        <#default>
            black
    </#switch>
</#macro>


<#function findBy items key value>
    <#list items as item>
        <#if item[key] == value>
            <#return item/>
        </#if>
    </#list>
</#function>

<#function getIndex items key value>
    <#assign cnt = 0/>
    <#list items as item>
        <#if item[key] == value>
            <#return cnt/>
        </#if>
        <#assign cnt = cnt + 1/>
    </#list>
</#function>

<#macro userlinkSign user sign>
<#-- @ftlvariable name="user" type="ru.itmo.web.hw4.model.User" -->
    <style>
        .nav-button {
            padding-right: 1rem;
            font-size: 2rem;
            color: var(--border-color);
        }

        .nav-button a {
            text-decoration: none;
            color: blue;
        }
    </style>


    <div class="nav-button">
        <#if user?? && user?has_content>
            <a href="?handle=${user.handle}">${sign}</a>
        <#else>
            ${sign}
        </#if>
    </div>
</#macro>

<#function reverse list>
    <#assign result = []>
    <#list list as item>
        <#assign result = [item] + result>
    </#list>
    <#return result>
</#function>

<#macro onePostText post full>
    <#if (post.text?length > 350) && !full>
        ${post.text?substring(0, 350)}...
    <#else>
        ${post.text}
    </#if>
</#macro>

<#macro onePost post full>
        <article>
            <div class="title">${post.title}</div>
            <#assign cur_u = findBy(users, "id", post.userId)/>
            <div class="information">By <@userlink cur_u/>, 0</div>
            <div class="body">

                <p><@onePostText post full/></p>
            </div>
            <div class="footer">
                <div class="left">
                    <img src="img/voteup.png"/>
                    <span class="positive-score">+173</span>
                    <img src="img/votedown.png"/>
                </div>
                <div class="right">
                    <img src="img/date_16x16.png"/>
                    0
                    <img src="img/comments_16x16.png"/>
                    <a href="/post?post_id=${post.id}">View all</a>
                </div>
            </div>
        </article>
</#macro>

<#macro getAllPostsById use_id user_id>
    <#list reverse(posts) as post>
        <#if !use_id || user_id == post.userId>
            <@onePost post false/>
        </#if>
    </#list>
</#macro>

