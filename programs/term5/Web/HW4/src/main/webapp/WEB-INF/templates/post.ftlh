<#-- @ftlvariable name="post_id" type="java.lang.Long" -->
<#import "./common.ftlh" as c>

<@c.page>
    <#if post_id?? && post_id?has_content>
        <#assign cur_post = c.findBy(posts, "id", post_id)!/>
    </#if>

    <#if cur_post?has_content>
        <@c.onePost cur_post true/>
    <#else>
        No such page
    </#if>
</@c.page>