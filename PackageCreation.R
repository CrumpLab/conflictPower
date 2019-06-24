library(usethis)
create_package("../pname")
use_description_defaults()
use_gpl3_license()

use_roxygen_md()
use_readme_md()

pkgdown::build_site()
pkgdown::build_articles()
pkgdown::clean_site()
pkgdown::build_news()

use_news_md()

#function creation
use_r("multi_doc_compare")

use_rcpp(name = "rowSumsSq")

use_package("qdapRegex", "Suggests")
usethis::use_package("lsa")

usethis::use_pipe()

use_package_doc()
use_namespace()

use_vignette("Using_playjaReyesir")

use_pkgdown()



### create logo

library(hexSticker)
imgurl <- "xtra/cp.png"
sticker(imgurl, package="conflictPower", p_size=5.5, s_x=1, s_y=.85, s_width=.75,
        p_color ="#635860",
        h_color = "#635860",
        h_fill = "#f7f2f5",
        spotlight = TRUE,
        l_x = 1,
        l_y = 1.4,
        l_alpha = .4,
        l_height = 6,
        l_width = 6,
        url = "crumplab.github.io/conflictPower",
        u_x = 1, u_y = 0.08, u_color = "#635860",
        u_family = "Aller_Rg", u_size = 1.4, u_angle = 30,
        white_around_sticker = TRUE,
        filename="xtra/conflictpower.png",
        dpi=600)



