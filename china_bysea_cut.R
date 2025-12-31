library(sf)
library(dplyr)
ROOT_PATH <- "D:\\needs\\projects\\population_density"

root_folder <-"D:\\needs\\projects\\population_density\\Data\\China_by_sea"

shp_files <- list.files(
  path = root_folder,
  pattern = "\\.shp$",
  recursive = TRUE,
  full.names = TRUE
)

safe_read_and_set_crs <- function(file_path) {
  tryCatch({
    # 1. 读取文件
    sf_obj <- st_read(file_path, quiet = TRUE)
    
    # 2. 【核心】强制设置为 WGS84 (EPSG:4326)
    sf_obj <- st_set_crs(sf_obj, 4326)
    
    # 3. 添加一个列记录来源（可选）
    sf_obj$source_file <- basename(file_path)
    
    return(sf_obj)
  }, error = function(e) {
    warning(paste("读取文件失败:", file_path, "\n错误:", e$message))
    return(NULL)
  })
}

# 批量应用函数读取
sf_list <- map(shp_files, safe_read_and_set_crs) %>%
  compact() # 移除失败的(NULL)结果

cat("成功读取并设置了", length(sf_list), "个文件的CRS为EPSG:4326。\n")

unified_polygon <- bind_rows(sf_list) %>%
  st_union() %>%       # 执行融合操作
  st_sf() %>%
  mutate(name = "融合后的整体")


bd_admin <- st_transform(unified_polygon, 3106)

#注意坐标转换


# 2. 简化边界（减少计算复杂度）
bd_admin_simple <- st_simplify(bd_admin, dTolerance = 500)

# 3. 转换为3857坐标系获取边界框
bd_bbox_3857 <- st_bbox(st_transform(bd_admin_simple, 3857))
bbox_wkt_3857 <- st_as_text(st_as_sfc(bd_bbox_3857))

# 4. 分块读取数据（避免内存问题）
cat("正在分块读取人口数据...\n")

# 划分为4x4的网格
grid <- st_make_grid(bd_admin_simple, n = c(4, 4))
results <- list()

for (i in seq_along(grid)) {
  cat(sprintf("处理区块 %d/%d...\n", i, length(grid)))
  
  # 获取当前网格的边界框（3857坐标系）
  grid_3857 <- st_transform(st_as_sfc(st_bbox(grid[i])), 3857)
  grid_wkt_3857 <- st_as_text(grid_3857)
  
  # 读取当前区块
  chunk <- tryCatch({
    st_read(
      "D:\\needs\\projects\\population_density\\Data\\kontur_population_20231101.gpkg",
      wkt_filter = grid_wkt_3857,
      quiet = TRUE
    )
  }, error = function(e) {
    cat(sprintf("区块 %d 读取失败: %s\n", i, e$message))
    return(NULL)
  })
  
  if (!is.null(chunk) && nrow(chunk) > 0) {
    # 转换坐标系并裁剪
    chunk_3106 <- st_transform(chunk, 3106)
    chunk_filtered <- st_intersection(chunk_3106, bd_admin_simple)
    
    if (nrow(chunk_filtered) > 0) {
      results[[length(results) + 1]] <- chunk_filtered
    }
  }
  
  # 定期清理内存
  if (i %% 2 == 0) gc()
}

# 5. 合并结果
if (length(results) > 0) {
  bd_hex <- do.call(rbind, results)
  
  cat(sprintf("\n处理完成！\n"))
  cat(sprintf("提取到 %d 个六边形\n", nrow(bd_hex)))
  cat(sprintf("总人口: %.0f\n", sum(bd_hex$population, na.rm = TRUE)))
  
  # 保存结果
  st_write(bd_hex, 
           "D:\\needs\\projects\\population_density\\Data\\china_bysea_population_simple.gpkg",
           delete_dsn = TRUE)
  
  # 可视化
  plot(st_geometry(bd_admin_simple), main = "重庆人口六边形")
  plot(st_geometry(bd_hex), add = TRUE, col = "red", pch = 20, cex = 0.3)
} else {
  cat("未能提取到任何数据\n")
}

